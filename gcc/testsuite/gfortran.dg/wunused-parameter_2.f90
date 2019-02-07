! { dg-do compile }
! { dg-options "-Wunused-parameter -Wunused-dummy-argument" }
!
! PR fortran/52789
!
! Contributed by Mat Cross
!
!  Check for unused parameter and dummy argument
!

subroutine s(x)             ! { dg-warning "Unused dummy argument" }
integer, parameter :: i = 0 ! { dg-warning "Unused parameter" }
end
