! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/40728
!

! bogus error
SUBROUTINE s1
  IMPLICIT NONE
  real(4), volatile :: r4

  r4 = 0.0_4
  r4 = asinh(r4)         ! { dg-error "has no IMPLICIT type" }
END SUBROUTINE



! ICE on invalid (ATANH is defined by F2008 only)
SUBROUTINE s2
  IMPLICIT NONE
  real :: r
  r = 0.4
  print *, atanh(r)      ! { dg-error "has no IMPLICIT type" }
END SUBROUTINE
