! { dg-do compile }
! { dg-options "-std=f2003 -Wintrinsic-shadow -fall-intrinsics" }

! PR fortran/33141
! Check that the expected warnings are emitted if a user-procedure has the same
! name as an intrinsic, with -fall-intrinsics even regardless of std=*.

MODULE testmod
  IMPLICIT NONE

CONTAINS

  ! ASINH is one but not in F2003
  REAL FUNCTION asinh (arg) ! { dg-warning "shadow the intrinsic" }
    IMPLICIT NONE
    REAL :: arg
  END FUNCTION asinh

END MODULE testmod

! ACOSH not for F2003
REAL FUNCTION acosh (arg) ! { dg-warning "of an intrinsic" }
  IMPLICIT NONE
  REAL :: arg
END FUNCTION acosh

! We do only compile, so no main program needed.
