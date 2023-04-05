library(ggplot2)
library(dplyr)

# define your custom clustering algorithm function
my_cluster <- function(data, num_clusters) {
  
  # apply hierarchical clustering to the data
  hc <- hclust(dist(data))
  
  # cut the dendrogram into num_clusters clusters
  clusters <- cutree(hc, k = num_clusters)
  
  # convert the clusters into a data frame
  df <- data.frame(data, cluster = as.factor(clusters))
  
  # create a ggplot object with the data and the clusters
  plot <- ggplot(df, aes(x = Sepal.Length, y = Petal.Length, color = cluster)) +
    geom_point() +
    theme_minimal()
  
  # create polygons for each cluster
  for (i in 1:num_clusters) {
    
    # extract the data for the current cluster
    cluster_data <- df %>% filter(cluster == i)
    
    # compute the convex hull of the cluster
    hull <- chull(cluster_data$Sepal.Length, cluster_data$Petal.Length)
    hull_points <- cluster_data[hull, ]
    
    # create a data frame of angles between hull points and centroid
    centroid <- apply(hull_points[, c("Sepal.Length", "Petal.Length")], 2, mean)
    angles <- atan2(hull_points$Petal.Length - centroid[2], hull_points$Sepal.Length - centroid[1])
    angles_df <- data.frame(x = hull_points$Sepal.Length, y = hull_points$Petal.Length, angle = angles)
    angles_df$angle[angles_df$angle < 0] <- angles_df$angle[angles_df$angle < 0] + 2 * pi
    
    # sort the data frame by angle
    angles_df <- angles_df[order(angles_df$angle), ]
    
    # create a sequence of indices that represent the polygon
    indices <- numeric(0)
    last_index <- 1
    for (j in 2:nrow(angles_df)) {
      if ((angles_df$angle[j] - angles_df$angle[last_index]) < pi) {
        indices <- c(indices, last_index)
        last_index <- j
      }
    }
    indices <- c(indices, last_index, indices[1])
    
    # plot the polygon
    plot <- plot + geom_polygon(data = angles_df[indices, ], aes(x = x, y = y, fill = factor(i), colour = factor(i)), alpha = 0.2)
  }
  
  # return the plot
  return(plot)
}

# apply the custom clustering algorithm to the iris dataset
data <- iris[, c("Sepal.Length", "Petal.Length")]
plot <- my_cluster(data, 3)

# display the plot
plot
