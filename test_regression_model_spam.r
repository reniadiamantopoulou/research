# If it isn't installed, install the kernlab package
library(kernlab)
data(spam)
# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5) #random separate the data set
table(trainIndicator)


trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

#data summary
names(trainSpam)
head(trainSpam)
table(trainSpam$type)

#plots
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
plot(log10(trainSpam[, 1:4] + 1))

#clustering
#############
#
#[1] "make"              "address"           "all"               "num3d"             "our"               "over"              "remove"           
#[8] "internet"          "order"             "mail"              "receive"           "will"              "people"            "report"           
#[15] "addresses"         "free"              "business"          "email"             "you"               "credit"            "your"             
#[22] "font"              "num000"            "money"             "hp"                "hpl"               "george"            "num650"           
#[29] "lab"               "labs"              "telnet"            "num857"            "data"              "num415"            "num85"            
#[36] "technology"        "num1999"           "parts"             "pm"                "direct"            "cs"                "meeting"          
#[43] "original"          "project"           "re"                "edu"               "table"             "conference"        "charSemicolon"    
#[50] "charRoundbracket"  "charSquarebracket" "charExclamation"   "charDollar"        "charHash"          "capitalAve"        "capitalLong"      
#[57] "capitalTotal"      "type"              "numType"          


hCluster = hclust(dist(t(trainSpam[, 1:57]))) # 57 is the last rows number
plot(hCluster)
#new_clustering
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:57] + 1))))
plot(hClusterUpdated)

#prediction model
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5)) #if possibility >50% classify it as a spam
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
        lmFormula = reformulate(names(trainSpam)[i], response = "numType")
        glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
        cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

#measure of uncertainty
## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"
## Classification table
table(predictedSpam, testSpam$type)

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)