! { dg-do compile }
! { dg-options "-std=f95" }

! Type-bound procedures
! Test that F95 does not allow type-bound procedures

MODULE testmod
  IMPLICIT NONE

  TYPE t
    INTEGER :: x
  CONTAINS ! { dg-error "Fortran 2003" }
    PROCEDURE proc1 ! { dg-error "Fortran 2003" }
    PROCEDURE :: proc2 => p2 ! { dg-error "Fortran 2003" }
  END TYPE t

CONTAINS
  
  SUBROUTINE proc1 (me)
    IMPLICIT NONE
    TYPE(t1) :: me
  END SUBROUTINE proc1

  REAL FUNCTION proc2 (me, x)
    IMPLICIT NONE
    TYPE(t1) :: me
    REAL :: x
    proc2 = x / 2
  END FUNCTION proc2

END MODULE testmod
! { dg-excess-errors "no IMPLICIT type" }
