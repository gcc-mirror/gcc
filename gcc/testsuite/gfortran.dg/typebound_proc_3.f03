! { dg-do compile }
! { dg-options "-std=f2003" }

! Type-bound procedures
! Test that F2003 does not allow empty CONTAINS sections.

MODULE testmod
  IMPLICIT NONE

  TYPE t
    INTEGER :: x
  CONTAINS
  END TYPE t ! { dg-error "Fortran 2008" }

END MODULE testmod

! { dg-final { cleanup-modules "testmod" } }
