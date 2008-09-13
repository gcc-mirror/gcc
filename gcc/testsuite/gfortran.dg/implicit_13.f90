! { dg-do compile }

! PR fortran/35770
! Implicit declaration hides type of internal function.

! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>

IMPLICIT CHARACTER (s)
REAL :: RDA

RDA = S_REAL_SQRT_I(42) ! { dg-bogus "Can't convert" }

CONTAINS

REAL FUNCTION S_REAL_SQRT_I(I) RESULT (R)
  IMPLICIT NONE
  INTEGER :: I
  R = 0.0
END FUNCTION S_REAL_SQRT_I

END
