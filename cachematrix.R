## Together, the following functions cache the inverse of a matrix

## Creates a special matrix object that can cache an inverse ----
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the the inverse of the matrix returned by makeCacheMatrix ----
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
