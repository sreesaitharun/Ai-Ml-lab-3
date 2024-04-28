# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(factoextra)

# Read student data from a local CSV file
student_data <- read_csv("C:/Users/ASUS/Downloads/Tharun lab3.csv")

# Explore the structure and summary of the data
glimpse(student_data)
summary(student_data)

# Perform dimensionality reduction with PCA
pca_result <- student_data %>%
  select(studied_credits, num_of_prev_attempts) %>%
  scale() %>%
  princomp()

# Visualize the proportion of variance explained
fviz_eig(pca_result, addlabels = TRUE, main = "Scree Plot for PCA")

# Cluster the data using KMeans
kmeans_clusters <- kmeans(student_data[, c("studied_credits", "num_of_prev_attempts")], centers = 3, nstart = 20)

# Add cluster labels to the data frame
student_data$kmeans_cluster <- as.factor(kmeans_clusters$cluster)

# Visualize KMeans clusters
ggplot(student_data, aes(x = studied_credits, y = num_of_prev_attempts, color = kmeans_cluster)) +
  geom_point() +
  labs(title = "KMeans Clustering", x = "Studied Credits", y = "Number of Previous Attempts")

# Cluster the data using Hierarchical clustering (Ward's method)
hierarchical_clusters <- hclust(dist(student_data[, c("studied_credits", "num_of_prev_attempts")]), method = "ward.D2")

# Cut the dendrogram into clusters
cut_clusters <- cutree(hierarchical_clusters, k = 3)

# Add hierarchical cluster labels to the data frame
student_data$hierarchical_cluster <- as.factor(cut_clusters)

# Visualize hierarchical clusters
plot(hierarchical_clusters, main = "Hierarchical Clustering Dendrogram",
     xlab = "Students", ylab = "Distance")

# Explore the distribution of students within clusters for both KMeans and hierarchical clustering
cat("\nNumber of students in each KMeans cluster:\n")
table(student_data$kmeans_cluster)

cat("\nNumber of students in each hierarchical cluster:\n")
table(student_data$hierarchical_cluster)

# Consider the interpretability of the features associated with each principal component 
# Loadings of the first two principal components
loadings <- pca_result$loadings[, 1:2]

# Plot the loadings
fviz_pca_var(pca_result, axes = c(1, 2), col.var = "black")

# Interpretation
print("\nLoadings of the first two principal components:")
print(loadings)
