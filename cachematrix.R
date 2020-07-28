# Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there are
# some benefit to catching the inverse of a matrix rather than
# compute it repeateadly

# These pair of functions work together to create a special object
# that stores a matri and caches it inverse. 

# This function creates a special "matrix"object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL #creates empty vector
 set<-function(y){
   x<<-y
   inv<<-NULL
 }
 get<-function()x #get values of original matrix
 setInv<-function(inverse) inv<<-inverse 
 getInv<-function()inv #calculates the inverse of matrix
 list(set=set, get=get, setInv=setInv, getInv=getInv)
 #list allows the $ to access function by name
}


#This function calculates the inverse of the  matrix created by the function
#"makeCacheMatrix". If the inverse has already been computed it skips the
#computation and get the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        inv<-x$getInv()#returns a matrix that is inverse of x
        if(!is.null(inv)){ #find the inverse matrix stored of x
          message("getting cached data")
          return(inv) #print the inverse matrix
        }
        data<-x$get() 
        inv<-solve(data, ...)
        x$setInv(inv)
        inv
}
