! { dg-do compile }
! { dg-options "-Wno-intrinsic-shadow -fall-intrinsics" }

! PR fortran/33141
! Check that the "intrinsic shadow" warnings are not emitted if the warning
! is negated.

MODULE testmod
  IMPLICIT NONE

CONTAINS

  REAL FUNCTION asin (arg) ! { dg-bogus "shadow the intrinsic" }
    IMPLICIT NONE
    REAL :: arg
  END FUNCTION asin

END MODULE testmod

REAL FUNCTION acos (arg) ! { dg-bogus "of an intrinsic" }
  IMPLICIT NONE
  REAL :: arg
END FUNCTION acos

! We do only compile, so no main program needed.

! { dg-final { cleanup-modules "testmod" } }
