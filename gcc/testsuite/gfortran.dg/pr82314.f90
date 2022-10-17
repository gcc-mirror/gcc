! { dg-do run }
! PR fortran/82314 - ICE in gfc_conv_expr_descriptor

program p
  implicit none
  integer, parameter :: karray(merge(3,7,.true.):merge(3,7,.false.)) = 1
  integer, parameter :: i = size   (karray)
  integer, parameter :: l = lbound (karray,1)
  integer, parameter :: u = ubound (karray,1)
  if (l /= 3 .or. u /= 7 .or. i /= 5) stop 1
end
