! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Copied from finalize_6.f90 - was before rejected as the finalization
! wrapper uses TS29913 (-std=f2008ts) features.
!

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER :: fooarr(42)
    REAL :: foobar
  CONTAINS
    FINAL :: finalize_single
  END TYPE mytype

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
