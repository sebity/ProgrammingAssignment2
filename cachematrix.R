## This function creates a special "matrix" object that can cache
## its inverse.
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse value of the matrix
# 4. get the inverse value of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  # Set the matrix
  setMatrix <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  
  # Get the matrix
  getMatrix <- function() {
    x
  }
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # Get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  # Return a list of the methods
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the "matrix" returned by
## makeCacheMatrix function.
#
cacheSolve <- function(x, ...) {
  
  # Get the inverse of the matrix and assign it to 'm'
  m <- x$getInverse()
  
  # Return the inverse of the matrix if it has already been cached
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix and assign it to 'data'
  data <- x$getMatrix()
  
  # Calculate the inverse of the matrix using matrix multiplication
  m <- solve(data)
  
  # Set the inverse of the matrix
  x$setInverse(m)
  
  # Return the matrix
  return(m)
}


## Testing:
# > x = rbind(c(4, 3), c(3, 2)
# > m = makeCacheMatrix(x)
# > m$getMatrix()
#      [,1] [,2]
# [1,]    4    3
# [2,]    3    2
#
## The matrix is not cached on first run:
# > cacheSolve(m)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
#
## Returns inverse from the cache when run second time:
# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4