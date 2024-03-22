! { dg-do compile }
! { dg-additional-options "-std=f2018" }
!
! PR fortran/96580 - constraints on VALUES argument of DATE_AND_TIME intrinsic

program test_time_and_date
  implicit none
  integer(1), dimension(8) :: values1
  integer(2), dimension(8) :: values2
  integer(4), dimension(8) :: values
  integer(4), dimension(9) :: values4
  integer(8), dimension(8) :: values8
  integer   , dimension(7) :: values7

  call date_and_time(VALUES=values1) ! { dg-error "decimal exponent range" }
  call date_and_time(VALUES=values2)
  call date_and_time(VALUES=values)
  call date_and_time(VALUES=values4)
  call date_and_time(VALUES=values8)
  call date_and_time(VALUES=values7) ! { dg-error "at .1. too small \\(7/8\\)" }
end program test_time_and_date
