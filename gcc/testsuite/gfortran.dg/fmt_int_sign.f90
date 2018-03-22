! { dg-do run }
! { dg-options -fno-range-check }
! PR38504 double minus sign when printing integer
! Test case derived from example by Jos de Kloe
program IntAdtest

  integer, parameter :: i8_ = Selected_Int_Kind(18)  ! = integer*8
  character(len=22) :: str_value
  integer(i8_) :: value
  character(len=*), parameter :: format_IntAd  = "(i22)"

  value = -9223372036854775807_i8_ -1
  write(str_value, format_IntAd) value
  if (str_value.ne."  -9223372036854775808") STOP 1

end program IntAdtest
