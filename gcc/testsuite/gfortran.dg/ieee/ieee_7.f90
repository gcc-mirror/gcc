! { dg-do run }

  use :: ieee_arithmetic
  implicit none

  ! Test IEEE_SELECTED_REAL_KIND in specification expressions

  integer(kind=ieee_selected_real_kind()) :: i1
  integer(kind=ieee_selected_real_kind(10)) :: i2
  integer(kind=ieee_selected_real_kind(10,10)) :: i3
  integer(kind=ieee_selected_real_kind(10,10,2)) :: i4

  ! Test IEEE_SELECTED_REAL_KIND

  if (ieee_support_datatype(0.)) then
    if (ieee_selected_real_kind() /= kind(0.)) call abort
    if (ieee_selected_real_kind(0) /= kind(0.)) call abort
    if (ieee_selected_real_kind(0,0) /= kind(0.)) call abort
    if (ieee_selected_real_kind(0,0,2) /= kind(0.)) call abort
  end if

  if (ieee_support_datatype(0.d0)) then
    if (ieee_selected_real_kind(precision(0.)+1) /= kind(0.d0)) call abort
    if (ieee_selected_real_kind(precision(0.),range(0.)+1) /= kind(0.d0)) call abort
    if (ieee_selected_real_kind(precision(0.)+1,range(0.)+1) /= kind(0.d0)) call abort
    if (ieee_selected_real_kind(precision(0.)+1,range(0.)+1,2) /= kind(0.d0)) call abort
  end if

  if (ieee_selected_real_kind(0,0,3) /= -5) call abort
  if (ieee_selected_real_kind(precision(0.d0)+1) /= -1) call abort
  if (ieee_selected_real_kind(0,range(0.d0)+1) /= -2) call abort
  if (ieee_selected_real_kind(precision(0.d0)+1,range(0.d0)+1) /= -3) call abort

end
