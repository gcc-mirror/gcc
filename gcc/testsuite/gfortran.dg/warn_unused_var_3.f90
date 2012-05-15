! { dg-do compile }
! { dg-options "-Wunused-parameter" }
!
! PR fortran/31461
!
module util_mod
  integer, parameter :: i = 4
end module util_mod

program main
    use util_mod, only: i ! { dg-warning "Unused parameter .i. which has been explicitly imported" }
    integer, parameter :: j = 4 ! { dg-warning "Unused parameter .j. declared at" }
end program main
