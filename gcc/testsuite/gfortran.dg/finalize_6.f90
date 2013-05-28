! { dg-do compile }
! { dg-options "-std=f95" }

! Parsing of finalizer procedure definitions.
! Check that CONTAINS/FINAL in derived types is rejected for F95.

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER :: fooarr(42)
    REAL :: foobar
  CONTAINS ! { dg-error "Fortran 2003: CONTAINS block in derived type definition" }
    FINAL :: finalize_single ! { dg-error "Fortran 2003: FINAL procedure declaration|FINAL procedure 'finalize_single' at .1. is not a SUBROUTINE" }
  END TYPE mytype ! { dg-error "Fortran 2008: Derived type definition at .1. with empty CONTAINS section" }

CONTAINS

  SUBROUTINE finalize_single (el)
    IMPLICIT NONE
    TYPE(mytype) :: el
    ! Do nothing in this test
  END SUBROUTINE finalize_single

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Do nothing
END PROGRAM finalizer
