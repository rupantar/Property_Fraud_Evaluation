rm(list = ls())
# READING THE DATA  
ny_prop_value = read.csv('property_values_csv.csv',na.strings = c("",".","NA","Inf"))
ny_prop_ori = ny_prop_value
ny_prop_value = ny_prop_ori
head(ny_prop_ori)
colnames(ny_prop_ori)
# Data cleaning

# remove the Features that have no predictive power
# eg the last three Features


ny_prop_value$PERIOD = NULL
ny_prop_value$YEAR = NULL
ny_prop_value$VALTYPE = NULL

col_df = colnames(ny_prop_value)

# Let us check out the dimensions of the data set

dim(ny_prop_value)

# so there are about a million rows in the dataset with 29 variables

# Entity variables
# BLOCK, BLDGCL, TAXCLASS , STORIES , ZIP

percent_missing = NULL

# calculate the percentage of missing values for each column

for( i in col_df)
   percent_missing[i] =  round(mean(is.na(ny_prop_value[,i])),3)

missing_per =  as.data.frame(percent_missing)
missing_per$type = c("id",'cat','id','cat','name','cat','cat','num',
                     'num','num','cat','num','num','num','num','num',
                     'num','cat','num','num','num','num','num','num',
                     'num','num')
missing_per
#Getting rid of variables with more than 70% missing values 

ny_prop_value$EASEMENT = NULL
ny_prop_value$EXMPTCL = NULL
ny_prop_value$EXCD2 = NULL
ny_prop_value$EXLAND2 = NULL
ny_prop_value$EXTOT2 = NULL
ny_prop_value$AVLAND2 = NULL
ny_prop_value$AVTOT2 = NULL

colnames(ny_prop_value)

# build some vars 
# LTAREA = gives the total area of the lot
ny_prop_value$LTAREA = ny_prop_value$LTFRONT * ny_prop_value$LTDEPTH
# BLDAREA = gives the total area of the building

ny_prop_value$BLDAREA = ny_prop_value$BLDFRONT * ny_prop_value$BLDDEPT

# creating a new data frame that is not does not have the LTAREA = 0
new_ny_prop_value = ny_prop_value[ny_prop_value$LTAREA != 0,]

# calculating the value per lot area
new_ny_prop_value$ValPerLtArea = new_ny_prop_value$FULLVAL / new_ny_prop_value$LTAREA

# rounding off the data 
new_ny_prop_value$ValPerLtArea = round(new_ny_prop_value$ValPerLtArea,2)


# Create multiple variables

# creating a new variable which is FULVALUE/BLDAREA

new_ny_prop_value$ValPerBldArea = round(new_ny_prop_value$FULLVAL/new_ny_prop_value$BLDAREA,3)

new_ny_prop_value$AvtotPerAvLand = round(new_ny_prop_value$AVTOT / new_ny_prop_value$AVLAND,3)


colnames(new_ny_prop_value)

sum(is.na(new_ny_prop_value$ValPerLtArea))
sum(is.na(new_ny_prop_value$ValPerBldArea))
sum(is.na(new_ny_prop_value$AvtotPerAvLand))


sum(is.infinite(new_ny_prop_value$ValPerBldArea))
sum(is.infinite(new_ny_prop_value$AvtotPerAvLand))
sum(is.infinite(new_ny_prop_value$ExtotperExLand))

new_ny_prop_value$ValPerBldArea[is.infinite(new_ny_prop_value$ValPerBldArea)]  = "NA"
new_ny_prop_value$ValPerBldArea =  as.numeric(new_ny_prop_value$ValPerBldArea) 

new_ny_prop_value[is.nan(as.matrix(new_ny_prop_value))] = 'NA'


#Building entities
# Entity variables
# BLOCK, BLDGCL, TAXCLASS , STORIES , ZIP

library(plyr)

#converting to factor
new_ny_prop_value$BLOCK = factor(new_ny_prop_value$BLOCK)
new_ny_prop_value$ZIP = factor(new_ny_prop_value$ZIP)
new_ny_prop_value$STORIES = factor(new_ny_prop_value$STORIES)

# entity for the Building class entity
bldclsentity = ddply(new_ny_prop_value, .(BLDGCL),summarise, meanValBLDGL = round(mean(ValPerLtArea),2))

new_ny_prop_value= merge(new_ny_prop_value,bldclsentity,by = 'BLDGCL')

new_ny_prop_value$ValPerLtAreaByBLDGCL = round(new_ny_prop_value$ValPerLtArea/new_ny_prop_value$meanVal,3)

new_ny_prop_value$meanValBLDGL = NULL


# entity for the Taxclass entity
 
taxclsentity = ddply(new_ny_prop_value,.(TAXCLASS),summarise,meanvaltxcl = round(mean(ValPerLtArea,na.rm = TRUE),2))
new_ny_prop_value= merge(new_ny_prop_value,taxclsentity,by = 'TAXCLASS')
new_ny_prop_value$ValPerLtAreaByTaxcls = round(new_ny_prop_value$ValPerLtArea/new_ny_prop_value$meanvaltxcl,3)

new_ny_prop_value$meanvaltxcl = NULL

# entity for the ZIP variable

zipclsentity = ddply(new_ny_prop_value,.(ZIP),summarise, meanvalzipcls =  round(mean(ValPerLtArea),2))
new_ny_prop_value = merge(new_ny_prop_value,zipclsentity,by = 'ZIP')
new_ny_prop_value$ValPerLtAreaByZipcls = round(new_ny_prop_value$ValPerLtArea/new_ny_prop_value$meanvalzipcls,3)

new_ny_prop_value$meanvalzipcls = NULL


# entity for the number of stories

new_ny_prop_value$STORIES = factor(new_ny_prop_value$STORIES)
storiesentity = ddply(new_ny_prop_value,.(STORIES),summarise,meanvalstoriescls = round(mean(ValPerLtArea,na.rm = TRUE),2))
new_ny_prop_value = merge(new_ny_prop_value,storiesentity,by = 'STORIES')
new_ny_prop_value$ValPerLtAreaBystoriescls = round(new_ny_prop_value$ValPerLtArea/new_ny_prop_value$meanvalstoriescls,2)

new_ny_prop_value$meanvalstoriescls = NULL

# Let us use the other variable which is the ValPerBldArea
# entity for the Building class entity

bldclsentity = ddply(new_ny_prop_value, .(BLDGCL),summarise, meanValBLDGL = round(mean(ValPerBldArea,na.rm = TRUE),2))

new_ny_prop_value= merge(new_ny_prop_value,bldclsentity,by = 'BLDGCL')

new_ny_prop_value$ValPerBldAreaByBLDGCL = new_ny_prop_value$ValPerBldArea/new_ny_prop_value$meanValBLDGL

new_ny_prop_value$ValPerBldAreaByBLDGCL = round(new_ny_prop_value$ValPerBldAreaByBLDGCL,3)
new_ny_prop_value$meanValBLDGL = NULL



# entity for the Taxclass entity

taxclsentity = ddply(new_ny_prop_value,.(TAXCLASS),summarise,meanvaltxcl = round(mean(ValPerBldArea,na.rm = TRUE),2))
new_ny_prop_value= merge(new_ny_prop_value,taxclsentity,by = 'TAXCLASS')
new_ny_prop_value$ValPerBldAreaByTaxcls = new_ny_prop_value$ValPerBldArea/new_ny_prop_value$meanvaltxcl

new_ny_prop_value$ValPerBldAreaByTaxcls = round(new_ny_prop_value$ValPerBldAreaByTaxcls,3)
new_ny_prop_value$meanvaltxcl = NULL

 
# entity for the ZIP variable

zipclsentity = ddply(new_ny_prop_value,.(ZIP),summarise, meanvalzipcls =  round(mean(ValPerBldArea,na.rm = TRUE),2))
new_ny_prop_value = merge(new_ny_prop_value,zipclsentity,by = 'ZIP')
new_ny_prop_value$ValPerBldAreaByZipcls = round(new_ny_prop_value$ValPerBldArea/new_ny_prop_value$meanvalzipcls,3)

new_ny_prop_value$meanvalzipcls = NULL

# entity for the number of stories

storiesentity = ddply(new_ny_prop_value,.(STORIES),summarise,meanvalstoriescls = round(mean(ValPerBldArea,na.rm = TRUE),2))
new_ny_prop_value = merge(new_ny_prop_value,storiesentity,by = 'STORIES')
new_ny_prop_value$ValPerBldAreaBystoriescls = round(new_ny_prop_value$ValPerBldArea/new_ny_prop_value$meanvalstoriescls,3)
new_ny_prop_value$meanvalstoriescls  = NULL

colnames(new_ny_prop_value)

# new variable chosen is AvtotPerAvLand

# building variable for entity level bldclass

bldclsentity = ddply(new_ny_prop_value,.(BLDGCL),summarise, meanValBLDGL = round(mean(AvtotPerAvLand,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,bldclsentity,by = 'BLDGCL')
new_ny_prop_value$AvtotPerAvLandByBLDGCL = round(new_ny_prop_value$AvtotPerAvLand/new_ny_prop_value$meanValBLDGL,3)

new_ny_prop_value$meanValBLDGL = NULL


# building variable for entity level taxclass


taxclsentity = ddply(new_ny_prop_value,.(TAXCLASS),summarise,meanValtxcl = round(mean(AvtotPerAvLand,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,taxclsentity, by = 'TAXCLASS')
new_ny_prop_value$AvtotPerAvLandByTaxclass = round(new_ny_prop_value$AvtotPerAvLand/new_ny_prop_value$meanValtxcl,3)

new_ny_prop_value$meanValtxcl = NULL

# building variables for the entity level ZIP 

zipclsentity = ddply(new_ny_prop_value,.(ZIP),summarise, meanvalzipcls =  round(mean(AvtotPerAvLand,na.rm = TRUE),2))
new_ny_prop_value = merge(new_ny_prop_value,zipclsentity,by = 'ZIP')
new_ny_prop_value$AvtotPerAvLandByZipcls = round(new_ny_prop_value$AvtotPerAvLand/new_ny_prop_value$meanvalzipcls,3)

new_ny_prop_value$meanvalzipcls = NULL

# building variables for the entity level stories

storiesentity = ddply(new_ny_prop_value,.(STORIES),summarise,meanvalstoriescls = round(mean(AvtotPerAvLand,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,storiesentity,by = 'STORIES')
new_ny_prop_value$AvtotPerAvLandByStories = round(new_ny_prop_value$ValPerBldArea/new_ny_prop_value$meanvalstoriescls,3)
new_ny_prop_value$meanvalstoriescls  = NULL

# taking the next entity level which is the LTAREA 

# building variable for entity level bldclass

bldclsentity = ddply(new_ny_prop_value,.(BLDGCL),summarise, meanValBLDGL = round(mean(LTAREA,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,bldclsentity,by = 'BLDGCL')
new_ny_prop_value$LTAREAByBLDGCL = round(new_ny_prop_value$LTAREA/new_ny_prop_value$meanValBLDGL,3)

new_ny_prop_value$meanValBLDGL = NULL


# building variable for entity level taxclass


taxclsentity = ddply(new_ny_prop_value,.(TAXCLASS),summarise,meanValtxcl = round(mean(LTAREA,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,taxclsentity, by = 'TAXCLASS')
new_ny_prop_value$LTAREAByTaxcls = round(new_ny_prop_value$LTAREA/new_ny_prop_value$meanValtxcl,3)

new_ny_prop_value$meanValtxcl = NULL

# building variables for the entity level ZIP 

zipclsentity = ddply(new_ny_prop_value,.(ZIP),summarise, meanvalzipcls =  round(mean(LTAREA,na.rm = TRUE),2))
new_ny_prop_value = merge(new_ny_prop_value,zipclsentity,by = 'ZIP')
new_ny_prop_value$LTAREAByZip = round(new_ny_prop_value$LTAREA/new_ny_prop_value$meanvalzipcls,3)

new_ny_prop_value$meanvalzipcls = NULL

# building variables for the entity level stories

storiesentity = ddply(new_ny_prop_value,.(STORIES),summarise,meanvalstoriescls = round(mean(LTAREA,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,storiesentity,by = 'STORIES')
new_ny_prop_value$LTAREAByStories = round(new_ny_prop_value$LTAREA/new_ny_prop_value$meanvalstoriescls,3)
new_ny_prop_value$meanvalstoriescls  = NULL

# taking the next entity level which is the BLDAREA 


# building variable for entity level bldclass

bldclsentity = ddply(new_ny_prop_value,.(BLDGCL),summarise, meanValBLDGL = round(mean(BLDAREA,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,bldclsentity,by = 'BLDGCL')
new_ny_prop_value$BLDAREAByBLDGCL = round(new_ny_prop_value$BLDAREA/new_ny_prop_value$meanValBLDGL,3)

new_ny_prop_value$meanValBLDGL = NULL


# building variable for entity level taxclass


taxclsentity = ddply(new_ny_prop_value,.(TAXCLASS),summarise,meanValtxcl = round(mean(BLDAREA,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,taxclsentity, by = 'TAXCLASS')
new_ny_prop_value$BLDAREAByTaxcls = round(new_ny_prop_value$BLDAREA/new_ny_prop_value$meanValtxcl,3)

new_ny_prop_value$meanValtxcl = NULL

# building variables for the entity level ZIP 

zipclsentity = ddply(new_ny_prop_value,.(ZIP),summarise, meanvalzipcls =  round(mean(BLDAREA,na.rm = TRUE),2))
new_ny_prop_value = merge(new_ny_prop_value,zipclsentity,by = 'ZIP')
new_ny_prop_value$BLDAREAByZip = round(new_ny_prop_value$BLDAREA/new_ny_prop_value$meanvalzipcls,3)

new_ny_prop_value$meanvalzipcls = NULL

# building variables for the entity level stories

storiesentity = ddply(new_ny_prop_value,.(STORIES),summarise,meanvalstoriescls = round(mean(BLDAREA,na.rm = TRUE),3))
new_ny_prop_value = merge(new_ny_prop_value,storiesentity,by = 'STORIES')
new_ny_prop_value$BLDAREAByStories = round(new_ny_prop_value$BLDAREA/new_ny_prop_value$meanvalstoriescls,3)
new_ny_prop_value$meanvalstoriescls  = NULL

save(new_ny_prop_value,file = 'new_ny_data.Rdata')

load('new_ny_data.Rdata')

colnames(new_ny_prop_value)

data_new = new_ny_prop_value[,c(5,25:44)]

data_scaled = scale(data_new[,-1], center = TRUE, scale = TRUE)

data_scaled = cbind(data_new$BBLE,as.data.frame(data_scaled))


colnames(data_scaled)[1] = 'BBLE'

data_scaled$ValPerLtAreaByBLDGCL = NULL


data_scaled <-data_scaled[complete.cases(data_scaled),]

# model 1 is the principle component analysis

model1 = princomp(na.omit(data_scaled[,2:20]))
summary(model1)



plot(model1, type = 'l',main = "Scree plot")


# take the principal components along these 4 components

head(model1$scores)
data_maha <- as.data.frame(model1$scores[,1:4])
data_maha <- cbind(data_scaled$BBLE,data_maha)
colnames(data_maha)[1] = 'BBLE'


save(data_maha,file= 'data_maha.csv')

m.dist <- as.data.frame(mahalanobis(data_maha[,2:5], colMeans(data_maha[,2:5]), cov(data_maha[,2:5])))
colnames(m.dist) = 'Mahanolobis_Distance'
m.dist <- cbind(data_scaled$BBLE,m.dist)
colnames(m.dist)[1] = 'BBLE'


round(quantile(m.dist$Mahanolobis_Distance),3)

m.sorted<-sort(m.dist,decreasing = TRUE)

min(m.dist$Mahanolobis_Distance)

hist(m.dist$Mahanolobis_Distance[m.dist$Mahanolobis_Distance<0.2],
     main = 'Histogram of Mahalanobis',xlab = 'Mahalanobis distance')

sum(m.dist$Mahanolobis_Distance>1000)


library(plyr)
m.extremes <-arrange(m.dist[m.dist$Mahanolobis_Distance>1000,],desc(Mahanolobis_Distance))

new_ny_prop_value[new_ny_prop_value$BBLE ==  '3001990126P',]
new_ny_prop_value[1,]



