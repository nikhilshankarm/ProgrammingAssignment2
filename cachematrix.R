# X is the input matrix and M is the inverse of matrix solved from X or returned from cache.

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the inverse of matrix
# 2. get the value of the inverse of matrix
# 3. set the value of the inverse of matrix
# 4. get the value of the inverse of matrix

#Function 1 - to make cache matrix 
makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) M <<- inverse
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function 2 - to find inverse of matrix if cache not found. 
# If inverse of matrix is found in cache, the function returns the inverse from cache and skips solve.

cacheSolve <- function(X, ...) {
  M <- X$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setinverse(M)
  M
}
