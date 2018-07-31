df <- read.csv("data.csv", head=TRUE, sep=";")

toConvert <- sapply(df, is.factor)
dataNumeric <- sapply(df[,toConvert], unclass)

dfNumeric = df
dfNumeric[] <- data.matrix(dfNumeric)

head(dfNumeric, 5)

summary(dfNumeric)

df <- dfNumeric

### Sort by "data.12" and by "data.11", in desc order

head(df[order(-df$data.12, -df$data.11),],30)

sort(unique(df$data.12))

sort(unique(df$data.11))

nrow(df); nrow(na.omit(df)); sum(complete.cases(df)); dim(df)

cat("Observations: ", nrow(df))

cat("Variables: ", ncol(df))

cat("Complete observations (without missing values): ", nrow(na.omit(df)))

### Draw a box plot for "data.4" grouped by "data.14"

boxplot(data.4 ~ data.14, 
        data = df, na.action = NULL,
        xlab = "data.14",
        ylab = "data.4",
        main = "Box plot")

### Draw a histogram of "data.12"

h <- hist(df$data.12,
     freq=TRUE,
     density=10,
     ylab="Number of data",
     xlab="data.12",
     main="Histogram")

h$counts=h$counts/sum(h$counts)
plot(h,
     ylab="Probability",
     xlab="data.12",
     main="Histogram")

### Draw a histogram of "data.12" grouped by "data.14"

library(ggplot2)

ggplot(df, aes(x=data.12, fill=data.14)) +
    geom_histogram(binwidth=1.5,position = "dodge")

### Draw a graph showing the density function data.12 grouped by data.14

ggplot(df, aes(x=data.12, fill=data.14)) +
    geom_density(alpha=0.4)

### Calculate corr for all possible pairs of variables

corrMatrix <- cor(df, use="complete.obs")
corrMatrix

heatmap(corrMatrix, symm=TRUE)

##### Or using reshape2 library:

library(reshape2)
mCorrMatrix <- melt(corrMatrix)
mCorrMatrix

ggplot(data = mCorrMatrix, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

### Draw scatter plots of three pairs of the most correlated variables.

scatterCorr <- corrMatrix
scatterCorr[scatterCorr == 1] <- NA
scatterCorr <- na.omit(melt(scatterCorr))
head(scatterCorr[order(-scatterCorr$value),],6)

pairs(~data.14+
      data.4+
      data.7+
      data.6+
      data.13, 
      data=corrMatrix,
      lower.panel = NULL,
     main="Scatter plots for most correlated pairs")

pairs(corrMatrix)

### Which have strongest corr with "data.12"?

which(colnames(corrMatrix)=="data.12")

bestCorr <- cor(corrMatrix, use="complete.obs")[which(colnames(corrMatrix)=="data.12"),]

bestCorr[bestCorr == 1] <- NA
bestCorr <- na.omit(bestCorr)
head(bestCorr[order(-bestCorr)],1)

### Draw scatter plots of 3 variables with strongest corr with "data.12"

head(bestCorr[order(-bestCorr)],3)

pairs(~data.12+data.11+data.13+data.3,
     data=corrMatrix,
     lower.panel = NULL)