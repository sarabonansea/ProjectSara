## We are writing a pair of functions that cache the inverse of a 
#matrix

## This function creates a special "matrix" object that can cache
#its inverse
makeCacheMatrix <- function(m = matrix()) { #define the argument with default mode of "matrix"
  j<-NULL #initialize j as null (j will hold the value of the inverse)
  set<-function(matrix){ #define the set function
    m <<- matrix       # m is defined in parental enviroment and is retreived by the function get
    j <<- NULL
  }
  get <- function() {m} #return the matrix m
  setinverse<-function(inverse){j<<-inverse} #set the inverse of the matrix
  getinverse<- function(){j} #return the inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse) #list of the methods
}


## This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then 
#cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inv <- x$get() #get the matrix
  m <- solve(inv, ...)#calculate the inverse
  x$setinverse(m) #set the inverse
  m #return the matrix
}
#check the code
pmatrix<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
pmatrix$get()
pmatrix$getinverse()
cacheSolve(pmatrix)
