# For Coursera Assignment
# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly.
# 
#  Functions:
#   1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
#   2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
#      the inverse from the cache.
# 

# makeCacheMatrix : Create a special matrix that can cache its inverse
# Internal functions:
# 1. set : set the value of the matrix and also set the inverse to NULL
# 2. get : get the matrix and return it
# 3. setInverse : set the inverse for the matrix
# 4. getInverse : get the inverse for the matrix
#

makeCacheMatrix <- function (x=matrix()) {
  mat_inv<-NULL

  # set the matrix value and make inverse null
  set<-function (y){
    x<<-y
    mat_inv<<-NULL
  }
  #get function
  get<- function() x
  #setInverse function, inverse is calculated outside (in cacheSolve) not here
  setInverse <- function(inv) mat_inv<<-inv
  #getInverse return the inverse 
  getInverse <- function() mat_inv
  
  list(set=set, get=get, setInverse=setInverse,
       getInverse=getInverse)
  
}


# cacheSolve: return the inverse of the matrix from the cache
# in case it does not exist in cache its calculated and "set" so that it can be used in future
cacheSolve <- function (x=matrix(), ...) {
  inv<-x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
    data<-x$get()
    inv<-solve(data)
    x$setInverse(inv)
    return (inv)
  } 
}

# ##
# ## Test cases - Ignore
# ## 
# run_tests=TRUE
# 
# if (run_tests) {
#   x<-matrix(c(1,4,0,3),nrow=2,ncol=2)
#   #
#   # [,1] [,2]
#   # [1,]    1    0
#   # [2,]    4    3
#   #Make sure the matrix has an inverse
#   temp<-solve(x)
#   print(temp)
#   
#   #call makeCacheMatrix
#   y<-makeCacheMatrix(x)
#   
#   #call cacheSolve first call calculates and sets inverse
#   temp<-cacheSolve(y)
#   print(temp)
#   
#   #call cacheSolve again
#   # should return "getting cached data" string
#   temp<-cacheSolve(y)
#   print(temp)
# }
# 
# 
# 
