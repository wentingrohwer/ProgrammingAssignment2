## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##set the static variable i, which represent the inverse of matrix x. initialize it to NULL
  i <- NULL
  ##set function,let x be the value passed in, and set inverse equal to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##get function, just return the x
  get <- function() x
  ##setinverse function. pass in the inverse of the matrix and save it to i
  setinverse <- function(inverseOfx) i <<- inverseOfx
  ##return i
  getinverse <- function() i
  ## return a list object that has four fields, which are all functions, set, get, setinverse, getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##x is the special matrix object, not the matrix itself.For the matrix what x is based on, let's call it m. i.e.identical(x$get(),m) == TRUE
  ##try to access the inverse of the matrix m.
  i <- x$getinverse()
  ## if the inverse of the matrix has already been calculated, just return i, which is x$getinverse()
  if(!is.null(i)) {
    message("getting the inverse matrix")
    return(i)
  }
  ##otherwise, set data to the matrix itself, m.
  data <- x$get()
  ## assign i the value of the inverse matrix of data, which is m.
  i <- solve(data)
  ## cache the inverse matrix, i,e save it to the special matrix object.
  x$setinverse(i)
  ##return the inverse matrix
  i
}
