## Put comments here that give an overall description of what your
## functions do

## creates a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  #inverse matrix initialized
  invMatrix <- NULL
  
  #creating freevariable x
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  
  #set the inverse
  setInverse <- function(inverse) invMatrix <<- inverse
  
  #get the inverse
  getInverse <- function() invMatrix
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}



##returns the inverse matrix, if an inverse matrix is in cache it returns the cache
##else it will compute and return the inverse matrix.  Saving valuable computation time
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- inverse(data, ...)
  x$setInverse(invMatrix)
  invMatrix

}
