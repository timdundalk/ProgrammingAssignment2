## The following functions perform:
## 1) makeCacheMatrix:  create special matrix with ability to cache
## 2) cacheSolve:  compute inverse of matrix


## The following function makes a special matrix (really a function) which contains
## functions to:
## - set the matrix
## - get the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(mat) m <<- mat
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}


## the following function checks if the inverse of the matrix has already been
## calculated. if so, it retrieves i.
## if not, it computes it and stores it into cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
  
}
