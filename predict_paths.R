library(dplyr)

tree_traverse <- function(tr, 
                          dat, 
                          target, 
                          node, 
                          path,
                          classification = F,
                          classes = NULL){
  
  # Auxilliary function to traverse the tree inside add_node_values
  #
  # Args:
  #   tr: One tree from a randomForest model with node values and paths
  #   dat: data available at this node
  #   target: target values associated with dat
  #   node: current node
  #   path: current path to get to this node
  #   classification: Whether or not the target is discrete
  #   classes: list of unique classes for classification
  #
  # Returns:
  #   A decision tree with additional columns reflecting target value
  #   at each node.
  
  # Add target data to node
  if (classification){
    for (c in classes){
      tr[node,c] <- mean(target==c)
    }
  } else {
    tr[node, 'target'] <- mean(target)
  }
  tr[node, 'count'] <- length(target)
  
  # # Append the path
  path <- c(path, node)
  tr$path[[node]] <- path
  
  # Check if this is a terminal node
  if (tr$left.daughter[node] == 0){
    return(tr)
  }
  
  # Compute the split
  var <- paste(tr[node, 'split.var'])
  idx.left <- dat[, var] <= tr[node, 'split.point']
  idx.right <- !idx.left
  
  # Go to the next level of the tree
  tr <- tree_traverse(tr, dat[idx.left,], target[idx.left], tr[node, 'left.daughter'], path, classification, classes)
  tr <- tree_traverse(tr, dat[idx.right,], target[idx.right], tr[node, 'right.daughter'], path, classification, classes)
  
  return(tr)
}



add_node_values <- function(tr, dat, target, classification = T){
  # Determine the target value at each node of a decision tree
  #
  # Args:
  #   tr: One tree from a randomForest model
  #   dat: data used to compute target at each node (may be training data)
  #   target: target values associated with dat
  #   classification: Whether or not the target is discrete
  #
  # Returns:
  #   A decision tree with additional columns reflecting target value
  #   at each node.
  
  # Initialize additional columns
  if (classification){
    classes <- as.character(unique(target))
    for (c in classes){
      tr[c] <- 0
    }
  } else {
    classes <- NULL
    tr$target <- 0
  }
  tr$count <- 0
  
  # One more column to hold the path
  tr$path <- lapply(1:nrow(tr), function(x) 1)
  
  # Traverse the tree
  tree_traverse(tr, dat, target, 1, list(), classification, classes)
}


contributions_aux <- function(tr, data){
  # Auxilliary function for computing feature contributions
  # Get the contributions from a single column of tr
  # (possibly one class or the target value for regression)
  #
  # Args:
  #   tr: One tree from a randomForest model with node values and paths
  #   data: test data to get contributions for
  # 
  # Returns:
  #   data frame containing feature contributions for the observations
  #   in data
  
  
}

feature_contributions <- function(tr, data){
  # For each test point in data, determine the feature contributions
  # to the final 
  #
  # Args:
  #   tr: One tree from a randomForest model with node values and paths
  #   dat: data used to compute target at each node (may be training data)
  #
  # Returns:
  #   A data frame where rows are test cases from data and columns are 
  #   contributions from all possible features
  
  
}

