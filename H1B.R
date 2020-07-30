
setwd('C:\\Users\\fonso\\Desktop\\CS504\\Project\\R')

# Import into dataframe
data <- read.csv("h1b_kaggle.csv")


library(ggplot2)
library(tidyr)
library(data.table)
dim(data)

#Clear Na's
data <- na.omit(data)

head(data2016)

#Subset 2016 data
data2016 <- droplevels(subset(data, YEAR=='2016' | YEAR=='2015' | YEAR=='2014'))

data2016 <- separate(data2016, WORKSITE, into = c("City","State"), sep = ", ", extra = "merge")


#seperate out denied and certified data (sample out 18519 denied) then merge
data_denied <- subset(data2016, CASE_STATUS == 'DENIED')

data_certified <- subset(data2016, CASE_STATUS == 'CERTIFIED')
data_certified <- data_certified[sample(nrow(data_certified), nrow(data_denied)),]

df_train <- droplevels(rbind(data_certified,data_denied))

# Salary
df_train$Salary <- '150+'
df_train$Salary[df_train$PREVAILING_WAGE < 150000 & df_train$PREVAILING_WAGE >= 125000] <- '125-150'
df_train$Salary[df_train$PREVAILING_WAGE < 125000 & df_train$PREVAILING_WAGE >= 100000] <- '100-125'
df_train$Salary[df_train$PREVAILING_WAGE < 100000 & df_train$PREVAILING_WAGE >= 75000] <- '75-100'
df_train$Salary[df_train$PREVAILING_WAGE < 75000 & df_train$PREVAILING_WAGE >= 50000] <- '50-75'
df_train$Salary[df_train$PREVAILING_WAGE < 50000] <- '50'

#Region
df_train$Region <- 'Other'

######################## remove blank space

df_train$Region[df_train$State %in% c('NEW YORK', 'CONNECTICUT', 'VERMONT'
                                      ,'PENNSYLVANIA','NEW JERSEY', 'DELAWARE'
                                      ,'MARYLAND','DISTRICT OF COLUMBIA')] <- 'Mid Atlantic'

df_train$Region[df_train$State %in% c('CONNECTICUT', 'VERMONT', 'MASSACHUSETTS'
                                      ,'RHODE ISLAND','MAINE', 'NEW HAMPSHIRE')] <- 'Northeast'

df_train$Region[df_train$State %in% c('OHIO', 'MICHIGAN', 'INDIANA', 'KANSAS'
                                      ,'ILLINOIS','WISONSIN', 'MISSOURI', 'NEBRASKA'
                                      ,'IOWA','MINNESOTA', 'NORTH DAKOTA', 'SOUTH DAKOTA')] <- 'Mid West'

df_train$Region[df_train$State %in% c('VIRGINIA', 'West Virginia', 'TENNESSEE', 'VERMONT'
                                      ,'SOUTH CAROLINA', 'NORTH CAROLINA', 'LOUISIANA', 'MISSISSIPPI'
                                      ,'KENTUCKY','GEORGIA', 'FLORIDA', 'ARKANSAS','ALABAMA' )] <- 'South'

df_train$Region[df_train$State %in% c('TEXAS', 'OKLAHOMA', 'NEW MEXICO', 'ARIZONA')] <- 'South West'

df_train$Region[df_train$State %in% c('COLORADO', 'WYOMING', 'MONTANA', 'IDAHO', 'UTAH', 'NEVADA'
                                      ,'WASHINGTON', 'OREGON','CALIFORNIA')] <- 'West'

table(df_train$SOC_NAME)

df_jobs <-data.frame(table(df_train$SOC_NAME, df_train$Job_Type))


df_train$SOC_NAME <- as.character(df_train$SOC_NAME)
str(df_train$SOC_NAME)

levels(df_train$SOC_NAME)

# Job Type
library(stringr)
df_train$Job_Type<- 'Other'



df_train$Job_Type[str_detect(df_train$SOC_NAME, paste(c(".*COMPUTER*",".*SOFTWARE*")))] <- 'Software/Computer'

df_train$Job_Type[str_detect(df_train$SOC_NAME, paste(c(".*FINANCE*",".*FINANCIAL*", ".*BUDGET*")))] <- 'Finance'

df_train$Job_Type[str_detect(df_train$SOC_NAME, paste(c(".*ENGINEER*")))] <- 'Engineer'

df_train$Job_Type[str_detect(df_train$SOC_NAME, paste(c(".*DENTIST*", ".*SURGEON*", ".*PHARMACIST*")))] <- 'Medical'

df_jobs <- data.frame(table(df_train$Job_Type, df_train$SOC_NAME))

str_detect(" COMPUTER SYSTEMS ANALYSTS", ".*COMPU*")

'VIRGINIA' %in% States
States

unique(df_train$State)

########################3 Classification
# Rpart
library(rpart)
fit <- rpart(CASE_STATUS ~ FULL_TIME_POSITION + YEAR + Salary + State + Region +Job_Type,
             data=df_train,
             method="class")

plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

# Random Forest

install.packages('randomForest')
library(randomForest)

set.seed(415)

fit <- randomForest(as.factor(CASE_STATUS) ~ FULL_TIME_POSITION + YEAR + Salary + Region + Job_Type,
                    data=df_train, 
                    importance=TRUE, 
                    ntree=2000)



# write to CSV
write.csv(df_train, file = "MyData.csv")


#table for State
table(data2016$CASE_STATUS)



#table for Job Type
table <- table(droplevels(data2016$SOC_NAME))

table(data2016$CASE_STATUS)

#wage mean
mean(data2016$PREVAILING_WAGE, trim=.10)

hist(data2016$PREVAILING_WAGE, breaks=50, xlim=c(0,3000000))

range(data2016$PREVAILING_WAGE)

