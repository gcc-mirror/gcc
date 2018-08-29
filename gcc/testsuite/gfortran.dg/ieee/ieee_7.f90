! { dg-do run }

  use :: ieee_arithmetic
  use :: iso_fortran_env, only : real_kinds
  implicit none

  ! This should be 
  ! integer, parameter :: maxreal = maxval(real_kinds)
  ! but it works because REAL_KINDS happen to be in increasing order
  integer, parameter :: maxreal = real_kinds(size(real_kinds))

  ! Test IEEE_SELECTED_REAL_KIND in specification expressions

  integer(kind=ieee_selected_real_kind()) :: i1
  integer(kind=ieee_selected_real_kind(10)) :: i2
  integer(kind=ieee_selected_real_kind(10,10)) :: i3
  integer(kind=ieee_selected_real_kind(10,10,2)) :: i4

  ! Test IEEE_SELECTED_REAL_KIND

  if (ieee_support_datatype(0.)) then
    if (ieee_selected_real_kind() /= kind(0.)) STOP 1
    if (ieee_selected_real_kind(0) /= kind(0.)) STOP 2
    if (ieee_selected_real_kind(0,0) /= kind(0.)) STOP 3
    if (ieee_selected_real_kind(0,0,2) /= kind(0.)) STOP 4
  end if

  if (ieee_support_datatype(0.d0)) then
    if (ieee_selected_real_kind(precision(0.)+1) /= kind(0.d0)) STOP 5
    if (ieee_selected_real_kind(precision(0.),range(0.)+1) /= kind(0.d0)) STOP 6
    if (ieee_selected_real_kind(precision(0.)+1,range(0.)+1) /= kind(0.d0)) STOP 7
    if (ieee_selected_real_kind(precision(0.)+1,range(0.)+1,2) /= kind(0.d0)) STOP 8
  end if

  if (ieee_selected_real_kind(0,0,3) /= -5) STOP 9
  if (ieee_selected_real_kind(100*precision(0._maxreal)) /= -1) STOP 10
  if (ieee_selected_real_kind(0,100*range(0._maxreal)) /= -2) STOP 11
  if (ieee_selected_real_kind(100*precision(0._maxreal),100*range(0._maxreal)) /= -3) STOP 12

end
