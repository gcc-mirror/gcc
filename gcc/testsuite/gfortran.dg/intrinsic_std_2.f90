! { dg-do link }
! { dg-options "-std=f95 -Wintrinsics-std -fall-intrinsics" }

! PR fortran/33141
! Check that -fall-intrinsics makes all intrinsics available.

PROGRAM main
  IMPLICIT NONE

  ! abort is a GNU extension
  CALL abort () ! { dg-bogus "extension" }

  ! ASINH is an intrinsic of F2008
  WRITE (*,*) ASINH (1.) ! { dg-bogus "Fortran 2008" }
END PROGRAM main
