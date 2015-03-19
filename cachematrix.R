## Functions compute the inverse of a matrix 
## The assumption is that the input supplied is always invertible matrix


## This function takes a matrix input and return a list of functions. 
## The elements of the list are as follow:
  ## get(), a function that returns the input matrix
  ## set(), a function that takes a matrix and set the global value of the matrix thus invalidates the cache and reset the output value
  ## getInverse(), a function that returns the inverted matrix of the input
  ## setInverse(), a function sets the inverted matrix of the input


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
