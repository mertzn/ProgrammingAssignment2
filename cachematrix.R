## Put comments here that give an overall description of what your
## functions do
##  Assignment: Caching the Inverse of a Matrix

##  Write the following functions:

## Write a short comment describing this function
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y)  {
    x <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_x <<- inverse
  getinv <- function() inv_x
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  
  if(!is.null(inv_x)) {   ## Return chached matrix inverse but only if it exists
    message("Obtaining cached data")
    return(inv_x)
  }
  
  data <- x$get()     ##  Calculate and return matrix inverse but only if it doesn't already exist
  inv_x <- solve(data, ...)
  x$setinv(inv_x)
  inv_x
}
