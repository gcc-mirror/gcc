! { dg-do compile }
! Tests the fix for PR39295, in which the check of the interfaces
! at lines 26 and 43 failed because opfunc1 is identified as a 
! function by usage, whereas opfunc2 is not. This testcase checks
! that TKR is stll OK in these cases.
!
! Contributed by Jon Hurst <jhurst@ucar.edu>
!
MODULE  funcs
CONTAINS
  INTEGER FUNCTION test1(a,b,opfunc1) 
    INTEGER :: a,b
    INTEGER, EXTERNAL :: opfunc1
    test1 = opfunc1( a, b ) 
  END FUNCTION test1
  INTEGER FUNCTION sumInts(a,b)
    INTEGER :: a,b
    sumInts = a + b
  END FUNCTION sumInts
END MODULE funcs

PROGRAM test
  USE funcs 
  INTEGER :: rs
  INTEGER, PARAMETER :: a = 2, b = 1
  rs = recSum( a, b, test1, sumInts ) ! { dg-error "Type/rank mismatch in argument" }
  write(*,*) "Results", rs
CONTAINS
  RECURSIVE INTEGER FUNCTION recSum( a,b,UserFunction,UserOp ) RESULT( res )
    IMPLICIT NONE
    INTEGER :: a,b
    INTERFACE 
       INTEGER FUNCTION UserFunction(a,b,opfunc2) 
         INTEGER :: a,b
         REAL, EXTERNAL :: opfunc2
       END FUNCTION UserFunction
    END INTERFACE
    INTEGER, EXTERNAL :: UserOp 

    res = UserFunction( a,b, UserOp ) ! { dg-error "Type/rank mismatch in return value" }

    if( res .lt. 10 ) then
       res = recSum( a, res, UserFunction, UserOp ) 
    end if
  END FUNCTION recSum
END PROGRAM test

! { dg-final { cleanup-modules "funcs" } }
