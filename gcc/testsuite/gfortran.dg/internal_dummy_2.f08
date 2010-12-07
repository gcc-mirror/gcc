! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! PR fortran/34162
! Internal procedures as actual arguments (like restricted closures).
! Check it works basically.

! Contributed by Daniel Kraft, d@domob.eu.

MODULE m
  IMPLICIT NONE

  ABSTRACT INTERFACE
    FUNCTION returnValue ()
      INTEGER :: returnValue
    END FUNCTION returnValue

    SUBROUTINE doSomething ()
    END SUBROUTINE doSomething
  END INTERFACE

CONTAINS

  FUNCTION callIt (proc)
    PROCEDURE(returnValue) :: proc
    INTEGER :: callIt

    callIt = proc ()
  END FUNCTION callIt

  SUBROUTINE callSub (proc)
    PROCEDURE(doSomething) :: proc

    CALL proc ()
  END SUBROUTINE callSub

END MODULE m

PROGRAM main
  USE :: m
  IMPLICIT NONE

  INTEGER :: a

  a = 42
  IF (callIt (myA) /= 42) CALL abort ()

  CALL callSub (incA)
  IF (a /= 43) CALL abort ()

CONTAINS

  FUNCTION myA ()
    INTEGER :: myA
    myA = a
  END FUNCTION myA

  SUBROUTINE incA ()
    a = a + 1
  END SUBROUTINE incA

END PROGRAM main

! { dg-final { cleanup-modules "m" } }
