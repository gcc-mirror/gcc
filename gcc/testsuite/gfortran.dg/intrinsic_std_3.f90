! { dg-do link }
! { dg-options "-std=gnu -Wintrinsics-std" }

! PR fortran/33141
! -std=gnu should allow every intrinsic.

PROGRAM main
  IMPLICIT NONE

  ! abort is a GNU extension
  CALL abort () ! { dg-bogus "extension" }

  ! ASINH is an intrinsic of F2008
  WRITE (*,*) ASINH (1.) ! { dg-bogus "Fortran 2008" }
END PROGRAM main
