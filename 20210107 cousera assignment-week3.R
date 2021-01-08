# makeCacheMatrix creates a list containing a function, it can
makeCacheMatrix <- function(x = matrix())
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
{ i <- NULL
  set <- function(y) 
   { x <<- y
     i <<- NULL}
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve returns the inverse of the matrix. If the inverse has already been computed, this function gets the result and skips the computation. If not, it computes the inverse, sets the value in the cache.
cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()
  if(!is.null(i)) 
    { message("getting cached data")
    return(i) }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

