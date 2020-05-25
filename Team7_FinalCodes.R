################################################################################
# MGMT 590: Predictive Analytics HW#2
################################################################################
set.seed(1234)
################################################################################
# Data loading, function loading, cleaning, and pre-processing chunk
################################################################################
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB
setwd("/home/jchittar") 
# load custom functions - you need to put these functions on your linux machine,
# or just run the four code scripts in RStudio Server
source("dumSymp.R")
source("DataQualityReportOverall.R")
source("DataQualityReport.R")
source("highCorr.R")

# Connecting to a MySQL on the server
#install.packages("odbc")
#install.packages("RMariaDB")
library(RMariaDB)
# Connect to a MariaDB version of a MySQL database

con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
                 , dbname="cancer"
                 , user="cancer_user", password="cancersux")

# query cancer2 dataset
d <- dbGetQuery(con, "SELECT * FROM cancer")
str(d)

# remove whitespaces from data load
d$id <- trimws(d$id, which = c("both"))
d$t_score <- trimws(d$t_score, which = c("both")) 
d$n_score <- trimws(d$n_score, which = c("both"))       
d$m_score <- trimws(d$m_score, which = c("both"))       
d$stage   <- trimws(d$stage, which = c("both"))     
d$race    <- trimws(d$race, which = c("both"))        
d$previous_cancer <- trimws(d$previous_cancer, which = c("both"))   
d$smoker <- trimws(d$smoker, which = c("both"))         
d$side <- trimws(d$side, which = c("both"))          
d$tumor_6_months <- trimws(d$tumor_6_months, which = c("both"))   
d$psa_6_months <- trimws(d$psa_6_months, which = c("both"))      
d$symptoms <- trimws(d$symptoms, which = c("both"))         
d$rd_thrpy <- trimws(d$rd_thrpy, which = c("both"))         
d$h_thrpy <- trimws(d$h_thrpy, which = c("both"))        
d$chm_thrpy <- trimws(d$chm_thrpy, which = c("both"))        
d$cry_thrpy <- trimws(d$cry_thrpy, which = c("both"))         
d$brch_thrpy <- trimws(d$brch_thrpy, which = c("both"))         
d$rad_rem <- trimws(d$rad_rem, which = c("both"))       
d$multi_thrpy <- trimws(d$multi_thrpy, which = c("both"))      
d$survival_1_year <- trimws(d$survival_1_year, which = c("both"))   
d$survival_7_years <- trimws(d$survival_7_years, which = c("both"))
str(d)

# define data types correctly
d$id <- as.factor(d$id)
d$diagnosis_date <- as.factor(d$diagnosis_date) 
d$t_score <- as.factor(d$t_score) 
d$n_score <- as.factor(d$n_score)   
d$m_score <- as.factor(d$m_score)      
d$stage   <- as.factor(d$stage)  
d$race    <- as.factor(d$race)    
d$previous_cancer <- as.factor(d$previous_cancer)  
d$smoker <- as.factor(d$smoker)        
d$side <- as.factor(d$side)         
d$tumor_6_months <- as.numeric(d$tumor_6_months)   
d$psa_6_months <- as.numeric(d$psa_6_months)      
d$symptoms <- as.factor(d$symptoms)       
d$rd_thrpy <- as.factor(d$rd_thrpy)        
d$h_thrpy <- as.factor(d$h_thrpy)     
d$chm_thrpy <- as.factor(d$chm_thrpy)       
d$cry_thrpy <- as.factor(d$cry_thrpy)       
d$brch_thrpy <- as.factor(d$brch_thrpy)        
d$rad_rem <- as.factor(d$rad_rem)     
d$multi_thrpy <- as.factor(d$multi_thrpy)  
d$survival_1_year <- as.factor(d$survival_1_year) 
d$survival_7_years <- as.factor(d$survival_7_years)
str(d)

# Jeff's codes
d$t_score <- factor(d$t_score,ordered = TRUE, levels = c("T1a","T1b","T1c","T2a","T2b","T2c","T3a","T3b","T3c","T4"))
d$m_score <- factor(d$m_score,ordered = TRUE,levels = c("M0","M1a","M1b","M1c"))
d$stage <- factor(d$stage,ordered = TRUE, levels = c("I","IIA","IIB","III","IV"))

d$t_score <- as.numeric(d$t_score)
d$m_score <- as.numeric(d$m_score)
d$stage <- as.numeric(d$stage)

# fix dataset so there are atomic values by creating a dummy feature for each
# symptom.
table(d$symptoms)
library(stringr)
s <- data.frame(str_split_fixed(d$symptoms, ",", 9))
source("dumSymp.R")
d$O01 <- as.factor(dumSymp(dataSet=s, myCode="O01"))
d$O08 <- as.factor(dumSymp(dataSet=s, myCode="O08"))
d$O09 <- as.factor(dumSymp(dataSet=s, myCode="O09"))
d$O10 <- as.factor(dumSymp(dataSet=s, myCode="O10"))
d$O11 <- as.factor(dumSymp(dataSet=s, myCode="O11"))
d$P01 <- as.factor(dumSymp(dataSet=s, myCode="P01"))
d$P02 <- as.factor(dumSymp(dataSet=s, myCode="P02"))
d$P03 <- as.factor(dumSymp(dataSet=s, myCode="P03"))
d$S04 <- as.factor(dumSymp(dataSet=s, myCode="S04"))
d$S07 <- as.factor(dumSymp(dataSet=s, myCode="S07"))
d$S10 <- as.factor(dumSymp(dataSet=s, myCode="S10"))
d$U01 <- as.factor(dumSymp(dataSet=s, myCode="U01"))
d$U02 <- as.factor(dumSymp(dataSet=s, myCode="U02"))
d$U03 <- as.factor(dumSymp(dataSet=s, myCode="U03"))
d$U05 <- as.factor(dumSymp(dataSet=s, myCode="U05"))
d$U06 <- as.factor(dumSymp(dataSet=s, myCode="U06"))
# remove symptoms feature
d$symptoms <- NULL
rm(s)
str(d)

# investigate data quality/missing valus
source("DataQualityReportOverall.R")
source("DataQualityReport.R")
DataQualityReportOverall(d) # percent complete data 
DataQualityReport(d)        # data quality by feature

# remove features with missing values more than 50% missing
d$tumor_6_months <- NULL
d$psa_6_months <- NULL

# do not impute values, just remove incomple records
DataQualityReport(d)
d <- na.omit(d)
DataQualityReport(d)

# investigate correlations
corMatrix <- cor(d[,c(3,8,10:13,17:21)])
# show me top correlated variables only - doesn't appear there are any here.
source("highCorr.R")
highCorr(corMatrix, cut_off=0.75)

# plot the two highly correlated features against the response. Decide which
# feature to keep and remove
#plot(d$survival_7_years ~ d$psa_1_year)
#plot(d$survival_7_years ~ d$psa_diagnosis)
#summary(glm(d$survival_7_years ~ d$psa_1_year, family = "binomial"))
#summary(glm(d$survival_7_years ~ d$psa_diagnosis, family = "binomial"))
#hist(d$psa_1_year)
#hist(d$psa_diagnosis)

# decided to keep psa_1_year because it is the newwest measurement a person
# would receive. People are diagnosed, then measured again after one year.
d$psa_diagnosis <- NULL

# remove id column
d$id <- NULL

# remove data column. Each row represents one patient and there are not multiple
# values over time for patients
d$diagnosis_date <- NULL

# put response in first column
names(d)
d <- data.frame(d$survival_7_years, d[,c(1:26,28:43)])
names(d)[1] <- "y"
str(d)
################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to take a 'factor variable' or 'categorical variable' and create
# columns for each factor level.
library(caret)
dummies <- dummyVars(y ~ ., data = d)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- cbind(d$y, ex)                              # combine your target variable with Xs
names(d)[1] <- "y"                               # make target variable called 'Y'
rm(dummies, ex)                                  # delete temporary things we no longer need
str(d)
################################################################################
# Identify Correlated Predictors and remove them
################################################################################
# If you build a model that has highly correlated independent variables it can
# lead to unstable models because it will tend to weight those more even though
# they might not be that important

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up
################################################################################
# Identifying linear dependencies and remove them
################################################################################
set.seed(1234) # set a seed so you can replicate your results
# Find if any linear combinations exist and which column combos they are.
# Below I add a vector of 1s at the beginning of the dataset. This helps ensure
# the same features are identified and removed.
library(caret)
# first save response
y <- d$y
# create a column of 1s. This will help identify all the right linear combos
d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] <- "ones"
# identify the columns that are linear combos
comboInfo <- findLinearCombos(d)
comboInfo
# remove columns identified that led to linear combos
d <- d[, -comboInfo$remove]
# remove the "ones" column in the first column
d <- d[, c(2:ncol(d))]
rm(comboInfo)

# Add the target variable back to our data.frame
d <- cbind(y, d)
################################################################################
## Zero- and Near Zero-Variance Predictors
################################################################################
# In some situations, the data generating mechanism can create predictors that 
# only have a single unique value (i.e. a "zero-variance predictor"). For many 
# models (excluding tree-based models), this may cause the model to crash or the 
# fit to be unstable.
# Similarly, predictors might have only a handful of unique values that occur 
# with very low frequencies. 

# identify columns that are "near zero"
nzv <- nearZeroVar(d[,2:ncol(d)])
# remove those columns from your dataset
d_filtered <- d[,2:ncol(d)][, -nzv]
# dimension of your filtered dataset
dim(d_filtered)
# updated dataset 
d <- cbind(d$y, d_filtered)
names(d)[1] <- "y"
# remove filtered dataset
rm(d_filtered)
################################################################################
# Standardize (and/ normalize) your input features.
################################################################################
# Here we standardize the input features (Xs) using the preProcess() function 
# by performing a min-max normalization (aka "range" in caret). We could use
# method=c("center","scale") to do a typical Z-score standardization

# In addition to standardizing the numeric features so they are on the same
# scale, we could choose to normalize (or make them more bell-shaped) as well.
# The "YeoJohnson" transformation is popular or the 'BoxCox" transform in lieu 
# of YeoJohnson if all your numeric features are >0 are reasonable choices.

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
d <- predict(preProcValues, d)
################################################################################
## END - Data loading, function loading, cleaning, and pre-processing chunk
################################################################################

################################################################################
# Data partitioning
################################################################################
library(caret)
set.seed(1234)

# make names for target if not already made
levels(d$y) <- make.names(levels(factor(d$y)))
# levels of a factor are re-ordered so that the level specified is first and 
# "X1" is what we are predicting (i.e. survival_7_years)
d$y <- relevel(d$y,"X1")

# partition data into training and testing
inTrain <- createDataPartition(y = d$y,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
# training and test data sets
tr <- d[inTrain,]
te <- d[-inTrain,]

################################################################################
# Predictive modeling
################################################################################
# Here we specify how to train models
# method: use "none" is for validation set approach
#         use "cv" & number= to specify k-fold cross-validation
# Regression vs. Classification type problems
# 1) Regregression: set classProbs = F  and summaryFunction = defaultSummary
# 2) Classification: set classProbs = T and summaryFunction = twoClassSummary
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,       # k number of times to do k-fold
                     classProbs = T,  
                     summaryFunction = twoClassSummary,
                     allowParallel=T
                     #completely option (used if you need to replicate results)
                     #seeds = list(c(1:10),c(11:20),c(21:30),c(31:40)
                     #             ,c(41:50),c(51:60),c(61:70),c(71:80)
                     #             ,c(81:90),c(91:100),c(101:110))
)
names(train)

# distribution of the response variable
table(d$y)

logit_model = train(y ~ ., 
                    data = tr, 
                    trControl = ctrl, 
                    method = "glm", family="binomial", metric="ROC")

varImp(logit_model)

tr2 <- subset(tr, select = c("y","n_scoreN1",
                             "gleason_score",
                             "U050",
                             "rd_thrpy1",
                             "S100",
                             "tumor_diagnosis",
                             "psa_1_year",
                             "rad_rem0",
                             "h_thrpy1",
                             "multi_thrpy0",
                             "race1",
                             "n_scoreN0",
                             "brch_thrpy1",
                             "S070",
                             "race2",
                             "first_degree_history",
                             "smoker0",
                             "weight",
                             "U010",
                             "previous_cancer0"))



############################################################################
# Feature selection using backward selection
################################################################################
#custom function that does backward selection on a logistic model
# LogitBackward = function(train, pval = 0.1) 
# {
#     train2 = tr
#     pmax = 1
#     worstdf = data.frame()
#     while (pmax>pval){
#         logit_model = train(y ~ ., 
#                             data = train2, 
#                             trControl = ctrl, 
#                             method = "glm", family="binomial", metric="ROC")
#         temp = data.frame(summary(logit_model)$coefficients)
#         worst = temp[temp$Pr...z..==max(summary(logit_model)$coefficients[,4]),]
#         if (worst$Pr...z..>0.1){
#             col1 = rownames(worst)
#             pmax = worst$Pr...z..
#             print(col1, pmax)
#             worstdf = rbind(worstdf,worst)
#             train2[col1] = NULL
#         } else {
#             pmax = 0
#         }
#     }
#     return (list(data = train2, removed = worstdf))
# }

# run backward selection function
# try1 = LogitBackward(tr, pval = 0.1) 
# tr2 = try1$data
# # features removed at each iteration from first to last one
# worstdf = try1$removed
# worstdf

# SAS Experiment
# a <- mean(tr$survival_1_year1)
# 
# a
# 
# tr$survival_1_year1 = tr$survival_1_year1 - a
# 
# b = 1/(1+tr$n_scoreN1)
# 
# tr$thing1 = tr$survival_1_year1*b

pl = 1/(1+tr2$psa_1_year)
ple = 1/(1+te$psa_1_year)

gl = 1/(1+tr2$gleason_score)
gle = 1/(1+te$gleason_score)

t = 1/(1+tr2$rd_thrpy1)
tt = 1/(1+te$rd_thrpy1)

tr2$thing1 = tr2$n_scoreN1*tr2$rd_thrpy1
te$thing1 = te$n_scoreN1*te$rd_thrpy1

tr2$thing2 = pl * gl
te$thing2 = ple * gle

tr2$thing3 = t*gl
te$thing3 = tt*gle

str(tr2)

# show final logit model using features from backward selection
# they should all be statistically significant at the alpha level you specified

logit <- train(y ~ .,              # model specification
               data = tr2,         # training set used to build model
               method = "glm",     # type of model you want to build
               trControl = ctrl,   # how you want to learn
               metric = "ROC",     # performance measure
               family="binomial")
summary(logit)
################################################################################
# Saving and reusing R models for later
################################################################################
# You can always save a model or dataset so you don't have to retrain or 
# recreate it later. To save a model try using the saveRDS() function from the
# mgcv package. When you run each line it will save the model you trained to your
# working directory.

# I commented these out since you haven't trained them yet in the script.
library(mgcv) # used to save models
saveRDS(svm, "jchittar_hw2_svm.rds")     
saveRDS(svmp, "jchittar_hw2_svmp.rds")  
saveRDS(svmr, "jchittar_hw2_svmr.rds")   
saveRDS(rf, "jchittar_hw2_rf.rds")       
#saveRDS(ada, "spring19_hw2_ada.rds")     
#saveRDS(ann, "spring19_hw2_ann.rds")     

# You can load them directly into your R environment exactly as they were created 
# using the readRDS() function.
readRDS(file="jchittar_hw2_svm.rds")    # loads back into R when ready to use
readRDS(file="jchittar_hw2_svmp.rds")   # loads back into R when ready to use
readRDS(file="jchittar_hw2_svmr.rds")   # loads back into R when ready to use
readRDS(file="jchittar_hw2_rf.rds")   # loads back into R when ready to use
readRDS(file="spring19_hw2_ada.rds")   # loads back into R when ready to use
readRDS(file="spring19_hw2_ann.rds")   # loads back into R when ready to use

################################################################################
# Basic Training and Evaluation Questions
################################################################################
# there are so many different types of models you can try, go here to see them all
# http://topepo.github.io/caret/available-models.html

# k-NN
knn <- train(y ~ .,
             data = tr2,         # training set used to build model
             method = "knn",     # type of model you want to build
             trControl = ctrl,   # how you want to learn
             metric = "ROC",     # performance measure
             tuneGrid = expand.grid(k = c(1:30)) # specific k's to try
)
knn

# Naive Bayes
nb <- train(y ~ .,               # model specification
            data = tr2,          # training set used to build model
            method = "nb",       # type of model you want to build
            trControl = ctrl,    # how you want to learn
            metric = "ROC"       # performance measure
)
nb

# Linear Discriminant Analysis
lda <- train(y ~ .,              # model specification
             data = tr2,         # training set used to build model
             method = "lda",     # type of model you want to build
             trControl = ctrl,   # how you want to learn
             metric = "ROC"      # performance measure
)
lda

# Support Vector Machine with a Linear Kernel
svm <- train(y ~ .,                  # model specification
             data = tr2,           # training set used to build model
             method = "svmLinear", # type of model you want to build
             trControl = ctrl,     # how you want to learn
             metric = "ROC",       # performance measure
             tuneGrid = expand.grid(C=c(0.1,0.25,0.5,0.75,1)) # tuning parameters to try
)
svm

# Support Vector Machine with a Polynomial Kernel
svmp <- train(y ~ .,                # model specification
              data = tr2,           # training set used to build model
              method = "svmPoly",   # type of model you want to build
              trControl = ctrl,     # how you want to learn
              metric = "ROC",       # performance measure
              tuneGrid = expand.grid(degree = c(3),
                                     scale = 0.01,
                                     C=1) # optimal tuning parameters
)
svmp


# Support Vector Machine with a Radial Kernel
svmr <- train(y ~ .,                # model specification
              data = tr2,           # training set used to build model
              method = "svmRadial", # type of model you want to build
              trControl = ctrl,     # how you want to learn
              metric = "ROC",       # performance measure
              tuneLength = 1:15     # tries 15 combinations of tuning parameters
              #tuneGrid = expand.grid(sigma=c(0.001, C=1) # tuning parameters to try
)
svmr

# Random Forest
rf <- train(y ~ .,               # model specification
            data = tr2,         # training set used to build model
            method = "rf",      # type of model you want to build
            trControl = ctrl,   # how you want to learn
            metric = "ROC",     # performance measure
            tuneLength = 1:15   # tries 15 combinations of tuning parameters
            #tuneGrid = expand.grid(mtry = c(1:30)) # tuning parameters to try
)
rf

# Boosted Classification Tree
ada <- train(y ~ .,                   # model specification
             data = tr2,              # training set used to build model
             method = "ada",          # type of model you want to build
             trControl = ctrl,        # how you want to learn
             metric = "ROC",          # performance measure
             #tuneLength = 15         # boosts 15 times
             tuneGrid = expand.grid(iter = c(10),  # no. of classifiers
                                    maxdepth = 6,  # max depth of tree
                                    nu = 1         # shrinkage parameter
             ) 
)
ada

# Artificial Neural Network
# size is the number of units in the hidden layer.
# decay is the parameter for weight decay. Default 0.
myGrid <-  expand.grid(size = c(3,5,10,20), decay = c(.05,.10),bag=FALSE)  
ann <- train(y ~ .,              # model specification
             data = tr2,         # training set used to build model
             method = "avNNet",    # type of model you want to build
             trControl = ctrl,   # how you want to learn
             maxit = 500,        # num of iterations
             tuneGrid = myGrid,  # tuning parameters to try
             metric = "ROC"      # performance measure
)
ann
################################################################################
# Capture the train and test estimated probabilities and predicted classes. Notice
# as you run each set of models how certain models take longer to "score".
# tr2<- tr
# logit model preds
trP_logit <- predict(logit, newdata=tr2, type='prob')[,1]
trC_logit <- predict(logit, newdata=tr2)
teP_logit <- predict(logit, newdata=te, type='prob')[,1]
teC_logit <- predict(logit, newdata=te)
# knn model preds
trP_knn <- predict(knn, newdata=tr2, type='prob')[,1]
trC_knn <- predict(knn, newdata=tr2)
teP_knn <- predict(knn, newdata=te, type='prob')[,1]
teC_knn <- predict(knn, newdata=te)
# nb model preds 
trP_nb <- predict(nb, newdata=tr2, type='prob')[,1]
trC_nb <- predict(nb, newdata=tr2)
teP_nb <- predict(nb, newdata=te, type='prob')[,1]
teC_nb <- predict(nb, newdata=te)
# lda model preds
trP_lda <- predict(lda, newdata=tr2, type='prob')[,1]
trC_lda <- predict(lda, newdata=tr2)
teP_lda <- predict(lda, newdata=te, type='prob')[,1]
teC_lda <- predict(lda, newdata=te)
# svm linear model preds
trP_svm <- predict(svm, newdata=tr2, type='prob')[,1]
trC_svm <- predict(svm, newdata=tr2)
teP_svm <- predict(svm, newdata=te, type='prob')[,1]
teC_svm <- predict(svm, newdata=te)
# svm poly model preds
trP_svmp <- predict(svmp, newdata=tr2, type='prob')[,1]
trC_svmp <- predict(svmp, newdata=tr2)
teP_svmp <- predict(svmp, newdata=te, type='prob')[,1]
teC_svmp <- predict(svmp, newdata=te)
# svm radial model preds
trP_svmr <- predict(svmr, newdata=tr2, type='prob')[,1]
trC_svmr <- predict(svmr, newdata=tr2)
teP_svmr <- predict(svmr, newdata=te, type='prob')[,1]
teC_svmr <- predict(svmr, newdata=te)
# random forest model preds
trP_rf <- predict(rf, newdata=tr2, type='prob')[,1]
trC_rf <- predict(rf, newdata=tr2)
teP_rf <- predict(rf, newdata=te, type='prob')[,1]
teC_rf <- predict(rf, newdata=te)
# adaBoost model preds
trP_ada <- predict(ada, newdata=tr2, type='prob')[,1]
trC_ada <- predict(ada, newdata=tr2)
teP_ada <- predict(ada, newdata=te, type='prob')[,1]
teC_ada <- predict(ada, newdata=te)
# ann model preds 
trP_ann <- predict(ann, newdata=tr2, type='prob')[,1]
trC_ann <- predict(ann, newdata=tr2)
teP_ann <- predict(ann, newdata=te, type='prob')[,1]
teC_ann <- predict(ann, newdata=te)

################################################################################
# Now use those predictions to assess performance on the training set and testing
# set. Be on the lookout for overfitting
# logit 
(trcm_logit <- confusionMatrix(data=trC_logit, tr2$y))
(tecm_logit <- confusionMatrix(data=teC_logit, te$y))
# knn
(trcm_knn <- confusionMatrix(data=trC_knn, tr2$y))
(tecm_knn <- confusionMatrix(data=teC_knn, te$y))
# nb 
(trcm_nb <- confusionMatrix(data=trC_nb, tr2$y))
(tecm_nb <- confusionMatrix(data=teC_nb, te$y))
# lda 
(trcm_lda <- confusionMatrix(data=trC_lda, tr2$y))
(tecm_lda <- confusionMatrix(data=teC_lda, te$y))
# svm linear
(trcm_svm <- confusionMatrix(data=trC_svm, tr2$y))
(tecm_svm <- confusionMatrix(data=teC_svm, te$y))
# svm polynomial
(trcm_svmp <- confusionMatrix(data=trC_svmp, tr2$y))
(tecm_svmp <- confusionMatrix(data=teC_svmp, te$y))
# svm radial
(trcm_svmr <- confusionMatrix(data=trC_svmr, tr2$y))
(tecm_svmr <- confusionMatrix(data=teC_svmr, te$y))
# rf
(trcm_rf <- confusionMatrix(data=trC_rf, tr2$y))
(tecm_rf <- confusionMatrix(data=teC_rf, te$y))
# ada
(trcm_ada <- confusionMatrix(data=trC_ada, tr2$y))
(tecm_ada <- confusionMatrix(data=teC_ada, te$y))
# ann 
(trcm_ann <- confusionMatrix(data=trC_ann, tr2$y))
(tecm_ann <- confusionMatrix(data=teC_ann, te$y))

################################################################################
# Q 13 evaluate metrics of all the models - train and test accuracy of all models were plot in Excel
################################################################################

# train and test statistics
trainStats <- rbind(
  t(rbind(data.frame(logit=trcm_logit$overall), data.frame(logit=trcm_logit$byClass))),
  t(rbind(data.frame(knn=trcm_knn$overall), data.frame(knn=trcm_knn$byClass))),
  t(rbind(data.frame(nb=trcm_nb$overall), data.frame(nb=trcm_nb$byClass))),
  t(rbind(data.frame(lda=trcm_lda$overall), data.frame(lda=trcm_lda$byClass))),
  t(rbind(data.frame(svm=trcm_svm$overall), data.frame(svm=trcm_svm$byClass))),
  t(rbind(data.frame(svmp=trcm_svmp$overall), data.frame(svmp=trcm_svmp$byClass))),
  t(rbind(data.frame(svmr=trcm_svmr$overall), data.frame(svmr=trcm_svmr$byClass))),
  t(rbind(data.frame(rf=trcm_rf$overall), data.frame(rf=trcm_rf$byClass))),
  t(rbind(data.frame(ada=trcm_ada$overall), data.frame(ada=trcm_ada$byClass))),
  t(rbind(data.frame(ann=trcm_ann$overall), data.frame(ann=trcm_ann$byClass)))
)
testStats <- rbind(
  t(rbind(data.frame(logit=tecm_logit$overall), data.frame(logit=tecm_logit$byClass))),
  t(rbind(data.frame(knn=tecm_knn$overall), data.frame(knn=tecm_knn$byClass))),
  t(rbind(data.frame(nb=tecm_nb$overall), data.frame(nb=tecm_nb$byClass))),
  t(rbind(data.frame(lda=tecm_lda$overall), data.frame(lda=tecm_lda$byClass))),
  t(rbind(data.frame(svm=tecm_svm$overall), data.frame(svm=tecm_svm$byClass))),
  t(rbind(data.frame(svmp=tecm_svmp$overall), data.frame(svmp=tecm_svmp$byClass))),
  t(rbind(data.frame(svmr=tecm_svmr$overall), data.frame(svmr=tecm_svmr$byClass))),
  t(rbind(data.frame(rf=tecm_rf$overall), data.frame(rf=tecm_rf$byClass))),
  t(rbind(data.frame(ada=tecm_ada$overall), data.frame(ada=tecm_ada$byClass))),
  t(rbind(data.frame(ann=tecm_ann$overall), data.frame(ann=tecm_ann$byClass)))
)

# here are your train and test stats for all models. Maybe you'll plot some of these
# to compare
accuracy <- table(trainStats[,c("Accuracy")], testStats[,c("Accuracy")])

m <- rbind(trainStats[,c("Accuracy")], testStats[,c("Accuracy")])

trainStats[,c("Accuracy")]
testStats[,c("Accuracy")]

class(m)

################################################################################
# Q 14 - plotting ROC curves
################################################################################
# Most analysts use ROC curves to compare models and help them decide which is
# better instead of using just the accuracy statistic
library(pROC)  #to generate ROC curves and capture AUC
roc_logit <- roc(response=te$y, predictor=teP_logit, levels=rev(levels(te$y)))
roc_knn <- roc(response=te$y, predictor=teP_knn, levels=rev(levels(te$y)))
roc_nb <- roc(response=te$y, predictor=teP_nb, levels=rev(levels(te$y)))
roc_lda <- roc(response=te$y, predictor=teP_lda, levels=rev(levels(te$y)))
roc_svm <- roc(response=te$y, predictor=teP_svm, levels=rev(levels(te$y)))
roc_svmp <- roc(response=te$y, predictor=teP_svmp, levels=rev(levels(te$y)))
roc_svmr <- roc(response=te$y, predictor=teP_svmr, levels=rev(levels(te$y)))
roc_rf <- roc(response=te$y, predictor=teP_rf, levels=rev(levels(te$y)))
roc_ada <- roc(response=te$y, predictor=teP_ada, levels=rev(levels(te$y)))
roc_ann <- roc(response=te$y, predictor=teP_ann, levels=rev(levels(te$y)))

# plot ROC curves (the curve closer to the upper left point is best)
par(mfrow=c(1,1), col="black", fg="black") 
plot(roc_logit, legacy.axes=T, col="red", main="ROC Curves")
lines(roc_knn, col="blue")
lines(roc_nb, col="green")
lines(roc_lda, col="purple")
lines(roc_svm, col="orange")
lines(roc_svmp, col="brown")
lines(roc_svmr, col="black")
lines(roc_rf, col="pink")
lines(roc_ada, col="lightblue")
lines(roc_ann, col="maroon")
legend("bottomright", title="Models", border="black", bty="o"
       , cex=.4, bg="lightgrey"
       , legend=c("logit","knn","nb","lda","svm","svmp","svmr","rf","ada","ann")
       , fill=c("red","blue","green","purple","orange","brown","black","pink"
                ,"lightblue","maroon"))

# AUC on test set data
aucResults <- data.frame(c("logit","knn","nb","lda","svm","svmp","svmr","rf","ada","ann")
                         ,c(auc(roc_logit)[1],auc(roc_knn)[1],auc(roc_nb)[1]
                            ,auc(roc_lda)[1],auc(roc_svm)[1],auc(roc_svmp)[1]
                            ,auc(roc_svmr)[1],auc(roc_rf)[1],auc(roc_ada)[1]
                            ,auc(roc_ann)[1]))
names(aucResults) <- c("Models","AUC")
(aucResults <- aucResults[order(-aucResults$AUC),])

################################################################################
# Q 15 - probability calibration plots
################################################################################
# investigate probability calibration plots
source("probCalPlot.R")
par(mfrow=c(2,3), col="black", fg="black") 
probCalPlot(Y=te$y, Yprobs=teP_logit, myModel="logit", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_knn, myModel="knn", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_nb, myModel="nb", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_lda, myModel="lda", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_svm, myModel="svm", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_svmp, myModel="svmp", numBins=20)
par(mfrow=c(2,3), col="black", fg="black") 
probCalPlot(Y=te$y, Yprobs=teP_svmr, myModel="svmr", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_rf, myModel="rf", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_ada, myModel="ada", numBins=20)
probCalPlot(Y=te$y, Yprobs=teP_ann, myModel="ann", numBins=20)

################################################################################
# Q 17 - Calculate specificity for all the models 
################################################################################

trainStats[,c("Specificity")]
testStats[,c("Specificity")]

################################################################################
# Q 18 Ensembling
################################################################################
# combine probability and class predictions together
train_p <- data.frame(trP_logit, trP_knn, trP_nb, trP_lda, trP_svm, trP_svmp,
                      trP_svmr, trP_rf, trP_ada, trP_ann)
test_p <- data.frame(teP_logit, teP_knn, teP_nb, teP_lda, teP_svm, teP_svmp,
                     teP_svmr, teP_rf, teP_ada, teP_ann)
train_c <- data.frame(trC_logit, trC_knn, trC_nb, trC_lda, trC_svm, trC_svmp,
                      trC_svmr, trC_rf, trC_ada, trC_ann)
test_c <- data.frame(teC_logit, teC_knn, teC_nb, teC_lda, teC_svm, teC_svmp,
                     teC_svmr, teC_rf, teC_ada, teC_ann)
head(train_p)
head(test_p)
head(train_c)
head(test_c)

# create ensembled responses to evaluate
source("vote.R")
train_maj = vote(data=train_c, type=1, k=5)
test_maj = vote(data=test_c, type=1, k=5)

#looking at the model stats for the voted models
confusionMatrix(train_maj,tr2$y)
confusionMatrix(test_maj,te$y)

# investigating other ways to vote
#train_maj = vote(data=train_c, type=2, k=7)
#test_maj = vote(data=test_c, type=2, k=7)
#confusionMatrix(train_maj,tr2$y)
#confusionMatrix(test_maj,te$y)

################################################################################
# Q 19 - identifiying the best values of k and type by running a loop
################################################################################

train_acc <- data.frame(type=integer(),k=integer(),accuracy=double())
test_acc <- data.frame(type=integer(),k=integer(),accuracy=double())

for (i in 1:10)
  for (j in 1:3)
{
    {
  train_maj = vote(data=train_c, type=j, k=i)
  test_maj = vote(data=test_c, type=j, k=i)
  x <- confusionMatrix(train_maj,tr2$y)
  train_acc <- rbind(train_acc,data.frame(j, i, x$overall['Accuracy']))
  y <- confusionMatrix(test_maj,te$y)
  test_acc <- rbind(test_acc,data.frame(j, i, y$overall['Accuracy']))
    }
  }

colnames(train_acc) = c("type","k","accuracy")
colnames(test_acc) = c("type","k","accuracy")

cbind(train_acc,test_acc$accuracy)

################################################################################
# Q 21 - finncial Costs of the models using svmp
################################################################################

TPc <- -2000
FPc <- 100000
FNc <- 1000
TNc <- -1000
costs <- matrix(c(TPc,FNc,FPc,TNc), byrow=F, nrow=2, ncol=2)

svmp_cost <- train(y ~ .,                # model specification
              data = tr2,           # training set used to build model
              method = "svmPoly",   # type of model you want to build
              trControl = ctrl,     # how you want to learn
              metric = "ROC",       # performance measure
              parms = list(split = "information", loss=costs),
              tuneGrid = expand.grid(degree = c(3),
                                     scale = 0.01,
                                     C=1) # optimal tuning parameters
)
svmp_cost

# svm poly model preds
trP_svmp_cost <- predict(svmp_cost, newdata=tr2, type='prob')[,1]
trC_svmp_cost <- predict(svmp_cost, newdata=tr2)
teP_svmp_cost <- predict(svmp_cost, newdata=te, type='prob')[,1]
teC_svmp_cost <- predict(svmp_cost, newdata=te)

(confusionMatrix(data=teC_svmp_cost, te$y))


#########################META MODEL START#####################################
# Q 20
#########################META MODEL START#####################################
#meta model
tr_metamodel = data.frame(tr2$y,trC_svmr,trC_svmp,trC_logit)
te_metamodel = data.frame(te$y,teC_svmr,teC_svmp,teC_logit)
names(tr_metamodel)=c("y","svmr","svmp","logit")
names(te_metamodel)=c("y","svmr","svmp","logit")

#Meta Model
myGrid <-  expand.grid(size = c(5,10), decay = c(.1,0.05), bag= FALSE)
ann_metamodel <- train(y ~ ., data = tr_metamodel, method = "avNNet",    
                       trControl = ctrl, maxit = 200, tuneGrid = myGrid,  
                       metric = "Spec"     
)

# meta model preds
trP_metamodel <- predict(ann_metamodel, newdata=tr_metamodel, type='prob')[,1]
trC_metamodel <- predict(ann_metamodel, newdata=tr_metamodel)
teP_metamodel <- predict(ann_metamodel, newdata=te_metamodel, type='prob')[,1]
teC_metamodel <- predict(ann_metamodel, newdata=te_metamodel)
trcm_metamodel <- confusionMatrix(data=trC_metamodel, tr2$y)
tecm_metamodel <- confusionMatrix(data=teC_metamodel, te$y)
trcm_metamodel
tecm_metamodel
