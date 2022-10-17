! { dg-do compile }
! { dg-options "-std=f95" }

! Type-bound procedures
! Test that F95 does not allow type-bound procedures

MODULE testmod
  IMPLICIT NONE

  TYPE t
    INTEGER :: x
  CONTAINS ! { dg-error "Fortran 2003: CONTAINS block in derived type definition" }
    PROCEDURE proc1 ! { dg-error "Fortran 2003: PROCEDURE statement" }
    PROCEDURE :: proc2 => p2 ! { dg-error "Fortran 2003: PROCEDURE statement" }
  END TYPE t                 ! { dg-error "Fortran 2008: Derived type definition at .1. with empty CONTAINS section" }

CONTAINS
  
  SUBROUTINE proc1 (me) ! { dg-error "no IMPLICIT type" }
    IMPLICIT NONE
    TYPE(t1) :: me     ! { dg-error "being used before it is defined" }
  END SUBROUTINE proc1

  REAL FUNCTION proc2 (me, x) ! { dg-error "no IMPLICIT type" }
    IMPLICIT NONE
    TYPE(t1) :: me     ! { dg-error "being used before it is defined" }
    REAL :: x
    proc2 = x / 2
  END FUNCTION proc2

END MODULE testmod
