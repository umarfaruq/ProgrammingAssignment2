## The purpose of this file is to enable us to cache inverse of a matrix. 
## It is helpful to cache the inverse as we dont need to calculate it repeatedly
## in case its computation intensive.

## ******USAGE******
## Create a matrix i.e.
## mymatrix = matrix(scan(), 3, 3)
## Optionally, create a variable to store function name for ease of use: 
## abc <- makeCacheMatrix()
## abc$set(my_matrix)
## abc$get()   will display the matrix cached
## cacheSolve(abc)   will calculate and cache the inverse if not already stored



## makeCacheMAtrix takes a matrix as input, and provides the following functions:
## 1. set: to cache the value of matrix
## 2. get: return the value of cached matrix. 
##    NA is returned if no matrix is cached
## 3. setinv: cache the value of matrix's inverse
## 4. getinv: return the value of inverse cached. 
##    NA is returned if inverse no set
## A vector of these functions is returned as a list


makeCacheMatrix <- function(x = matrix()) {
  ##tmp_inverse will store the cached inverse
  tmp_inverse <- NULL                     
  
  ## set the value, also set the inverse to NULL
  set <- function(var_mat) {
    x <<- var_mat
    tmp_inverse <<- NULL
  }
  
  ## return value of cached matrix
  get <- function() x
  
  ## cache the value of inverse
  setinv <- function(inverse) tmp_inverse <<- inverse
  
  ## return the value of inverse
  getinv <- function() tmp_inverse
  
  ## return a vector containing the above functions as a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if inverse is cached or not. If not, it will calculate and 
## cache the inverse. Finally it will return the inverse.

cacheSolve <- function(x, ...) {
  
  ## check if inverse is already cached, If yes, return its value
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## else get the matrix stored and calculate the inverse and store it. Finally
  ## return the calculated result
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
