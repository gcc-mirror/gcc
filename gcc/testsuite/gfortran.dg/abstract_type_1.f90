! { dg-do "compile" }
! { dg-options "-std=f95" }

! Abstract Types.
! Check that ABSTRACT is rejected for F95.

MODULE m

  TYPE, ABSTRACT :: t ! { dg-error "Fortran 2003" }
    INTEGER :: x
  END TYPE t ! { dg-error "END MODULE" }

END MODULE m
! { dg-final { cleanup-modules "m" } }
