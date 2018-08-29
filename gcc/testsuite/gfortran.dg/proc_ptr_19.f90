! { dg-do run }
!
! PR 40176:  Fortran 2003: Procedure pointers with array return value
!
! This example tests for a bug in procedure pointer assignments,
! where the rhs is a dummy.
!
! Original test case by Barron Bichon <barron.bichon@swri.org>
! Modified by Janus Weil <janus@gcc.gnu.org>

PROGRAM test_prog

  PROCEDURE(add), POINTER :: forig, fset

  forig => add

  CALL set_ptr(forig,fset)

  if (forig(1,2) /= fset(1,2)) STOP 1

CONTAINS

  SUBROUTINE set_ptr(f1,f2)
    PROCEDURE(add), POINTER :: f1, f2
    f2 => f1
  END SUBROUTINE set_ptr

  FUNCTION add(a,b)
    INTEGER :: a,b,add
    add = a+b

  END FUNCTION add
 
END PROGRAM test_prog

