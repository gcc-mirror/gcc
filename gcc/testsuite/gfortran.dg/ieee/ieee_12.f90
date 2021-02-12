! { dg-do run }
! PR95647 operator(.eq.) and operator(==) treated differently
program test
  use, intrinsic :: ieee_arithmetic, only :                 &
&                ieee_class,                                       &
&                ieee_class_type,                                  &
&                ieee_negative_normal,                             &
&                ieee_positive_normal,                             &
&                operator(.eq.), operator(.ne.)
  integer :: good
  real(4) r4
  type(ieee_class_type) class1
  good = 0
  r4 = 1.0
  class1 = ieee_class(r4)
  if (class1 .eq. ieee_positive_normal) good = good + 1
  if (class1 .ne. ieee_negative_normal) good = good + 1
  r4 = -1.0
  class1 = ieee_class(r4)
  if (class1 .eq. ieee_negative_normal) good = good + 1
  if (class1 .ne. ieee_positive_normal) good = good + 1
  if (good /= 4) call abort
end program test

