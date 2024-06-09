! { dg-do run }
! { dg-additional-options "-std=f2018" }
! { dg-require-effective-target fortran_integer_16 }
!
! PR fortran/96580 - integer kind of VALUES argument of DATE_AND_TIME intrinsic

program test_time_and_date
  implicit none
  integer(4), dimension(8) :: values4
  integer(8), dimension(8) :: values8
  integer(16),dimension(8) :: values16

  call date_and_time(VALUES=values4)
  call date_and_time(VALUES=values8)
  call date_and_time(VALUES=values16)

  ! Check consistency of year and of time difference from UTC
  if (values16(1) /= -HUGE(0_16) .and. values4(1) /= -HUGE(0_4)) then
     if (abs (values4(1) - values16(1)) > 1) stop 1
  end if
  if (values16(4) /= -HUGE(0_16) .and. values4(4) /= -HUGE(0_4)) then
     if (values16(4) /= values4(4))          stop 2
  end if
  if (values4(1) /= -HUGE(0_4) .and. values8(1) /= -HUGE(0_8)) then
     if (abs (values8(1) - values4(1)) > 1) stop 3
  end if
  if (values4(4) /= -HUGE(0_4) .and. values8(4) /= -HUGE(0_8)) then
     if (values4(4) /= values8(4))          stop 4
  end if
end program test_time_and_date
