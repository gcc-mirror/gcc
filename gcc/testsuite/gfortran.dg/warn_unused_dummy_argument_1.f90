! { dg-do compile }
! { dg-options "-Wall" }
!
! PR fortran/38407
!

SUBROUTINE s(dummy)                ! { dg-warning "Unused dummy" }
  INTEGER, INTENT(in) :: dummy
  INTEGER :: variable              ! { dg-warning "Unused variable" }
END SUBROUTINE
