# Agriculture-Machine-Learning-
As students from Universiti Tun Hussein Onn Malaysia (UTHM), we are conducting a research project that applies machine learning to agriculture.

setwd("~/")
install.packages(c("tidyverse","caret"))
library(tidyverse)
library(caret)

# 1. Load Data
data <- read_csv("C:\\Users\\USER\\Documents\\UTHM\\Group Work\\Y2S2 Machine Learning\\data_season.csv")

# View first rows and structure
head(data)
str(data)

# 2. Check Missing Values for each Column
colSums(is.na(data))

# 3. Remove all the rows with missing values
new_df <- na.omit(data)

# 4. Create yield class
new_df$yield_class <- cut(
     new_df$yields,
     breaks = quantile(new_df$yields,
                       probs = c(0, 0.33, 0.66, 1)),
     labels = c("Low", "Medium", "High"),
     include.lowest = TRUE
 )

# Convert to factor
new_df$yield_class <- as.factor(new_df$yield_class)

# 5. Normalization (Z-score Normalization for numerical columns)
normalized_df <- new_df
num_cols <- sapply(normalized_df, is.numeric)
# exclude the column "Year"
num_cols["Year"] <- FALSE
numeric_data <- normalized_df[, num_cols]
preprocess_model <- preProcess(numeric_data, method = c("center", "scale"))
scaled_data <- predict(preprocess_model, numeric_data)
normalized_df[, num_cols] <- scaled_data

head(normalized_df)
summary(normalized_df)
