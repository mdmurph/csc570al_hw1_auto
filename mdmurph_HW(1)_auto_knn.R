#print(getwd())

auto <- read.csv("auto.csv", stringsAsFactors = + FALSE)
str(auto)

#delete the rows with missing values in horsepower
auto<-auto[!(auto$horsepower=="?"),]
str(auto)

#convert horsepower from string to integer
auto$horsepower<-as.integer(auto$horsepower)

#add a mpg1 variable which is 1 if mpg > median_mpg, otherwise 0
#calculate median mpg
median_mpg<-median(auto$mpg)

#mpgtest function
mpgtest <- function(x) {
  testfunct <- 0
  if(x > median_mpg)
    testfunct<-1
  return (testfunct)
}

#copy mpg to a new vector mpgtestv
mpgtestv<-auto$mpg

#add a column called mpg1 to df auto
auto["mpg1"]<-0

#apply mpgtest function to mpgtestv vector and save in df auto as variable mpg1
auto$mpg1<-sapply(mpgtestv,mpgtest)

#test mpg1 calc
#summary(auto[c("mpg1")])

#select list of variables to drop from the model
drop.cols <- c("mpg","origin","name") #to remove multiple columns
auto<-auto[, !names(auto) %in% drop.cols, drop = F]

#set seed of random number generator
set.seed(1)

#shuffle df rowwise
auto_s <- auto[sample(nrow(auto)),]

#normalize the attribute values
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

auto_n <- as.data.frame(lapply(auto_s[1:6], normalize)) # columns, last is mpg1

#split into train and tests sets
auto_train <- auto_n[1:292, ]   #selects all columns of the # first 292 rows
auto_test  <- auto_n[293:392, ] #selects all columns of the # last 100 rows

#strip off labels
auto_train_labels <- auto_s[1:292, 7]   #selects the last # column from rows 1:292
auto_test_labels  <- auto_s[293:392, 7] #selects the last # column from rows 293:392

#install.packages(class)
library(class)

auto_test_pred <- knn(train = auto_train, test = auto_test,cl = auto_train_labels, k = 8)

#install.packages(class)
library(gmodels) # loads the gmodels package

CrossTable(x = auto_test_labels, y = auto_test_pred,prop.chisq=FALSE)

savehistory("script.txt")