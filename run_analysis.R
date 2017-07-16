library(dplyr)
library(data.table)
library(tidyr)

if(!file.exists("E:/R/data-cleaning")){dir.create("E:/R/data-cleaning")}
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "E:/R/data-cleaning/dataset.zip",method="curl") 
unzip ("E:/R/data-cleaning/dataset.zip", exdir = "E:/R/data-cleaning")

path<- file.path("E:/R/data-cleaning" , "UCI HAR Dataset")

# Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(path, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(path, "test" , "subject_test.txt" )))

# Read activity files
dataYTrain <- tbl_df(read.table(file.path(path, "train", "Y_train.txt")))
dataYTest  <- tbl_df(read.table(file.path(path, "test" , "Y_test.txt" )))

#Read data files.
dataXTrain <- tbl_df(read.table(file.path(path, "train", "X_train.txt" )))
dataXTest  <- tbl_df(read.table(file.path(path, "test" , "X_test.txt" )))

# for both Activity and Subject files this will merge the training and the test sets by row binding 
#and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataYTrain, dataYTest)
setnames(alldataActivity, "V1", "activityNum")

#combine the DATA training and test files
dataTable <- rbind(dataXTrain, dataXTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(path, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(path, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)


# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "Time", names(dataTable))
names(dataTable)<-gsub("^f", "Frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)



write.table(dataTable, file = "tidydata.txt",row.name=FALSE)

