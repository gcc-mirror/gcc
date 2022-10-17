! { dg-do compile }
! { dg-options "-std=f2018" }
! PR fortran/105138 - recursive procedures and shadowing of intrinsics

RECURSIVE FUNCTION LOG_GAMMA(Z) RESULT(RES)
  COMPLEX, INTENT(IN) :: Z
  COMPLEX             :: RES
  RES = LOG_GAMMA(Z)
END FUNCTION LOG_GAMMA

recursive subroutine date_and_time (z)
  real :: z
  if (z > 0) call date_and_time (z-1)
end subroutine date_and_time
