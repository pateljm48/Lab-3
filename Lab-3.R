library(dplyr)
library(tidyverse)  
library(factoextra)  
library(caret)
library(cluster)
library(ggplot2)

data <- read.csv("C:\\Users\\patel\\Downloads\\oulad-students.csv")

str(data)
summary(data)

data$final_result <- as.factor(data$final_result)
data$gender <- as.numeric(data$gender)
data$id_student <- as.numeric(data$id_student)


# Generate simulated learning data
set.seed(123)
n_students <- 1000
student_id <- 1:n_students
feature1 <- rnorm(n_students, mean = 706688, sd = 10)
feature2 <- rnorm(n_students, mean = 70, sd = 15)
data <- data.frame(Student_ID = student_id, Feature_1 = feature1, Feature_2 = feature2)

# Perform PCA for dimensionality reduction
pca <- prcomp(data[,c("Feature_1", "Feature_2")], center = TRUE, scale. = TRUE)

# Plot the PCA results
fviz_eig(pca) + ggtitle("Scree Plot")
fviz_pca_var(pca) + ggtitle("Variable Loading Plot")
fviz_pca_biplot(pca, repel = TRUE, title = "Biplot")

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(pca$x[,1:2], centers = 3, nstart = 25)

# Plot the clustering results
fviz_cluster(kmeans_result, data[,c("Feature_1", "Feature_2")]) + ggtitle("Clustering Results")

# Add cluster labels to the original data
data$Cluster <- as.factor(kmeans_result$cluster)

# Summary of the clustering results
summary(data$Cluster)