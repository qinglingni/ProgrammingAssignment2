## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Below function creates a special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
		inverse<-NULL
		set<-function(y) {
			x<<-y
			inverse<<-NULL
		}
		get<-function() x
		setinverse<-function(inv) inverse<<-inv
		getinverse<-function() inverse
		list(set=set,get=get,
		setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## Below function calculates the inverse of a matrix. It first checks if the inverse matrix has been created. If so, it gets the inverse. Otherwise it calculates the inverse of the matrix and sets the value of the inverse via setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        if(!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        }
        mtrx<-x$get()
        inverse<-solve(mtrx,...)
        x$setinverse(inverse)
        inverse
}
