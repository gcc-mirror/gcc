! { dg-do "compile" }
! { dg-options "-Wall -Wno-unused-dummy-argument" }
!
! PR fortran/38407
!

SUBROUTINE s(dummy)
  INTEGER, INTENT(in) :: dummy
  INTEGER :: variable              ! { dg-warning "Unused variable" }
END SUBROUTINE
