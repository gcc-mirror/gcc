! { dg-do compile }
! PR fortran/102817 - ICE in gfc_clear_shape

program test
  type t
     integer :: a(1,2) = 3
  end type t
  type(t), parameter :: u    = t(4)
  type(t), parameter :: x(1) = t(4)
  integer, parameter :: p(1,2) = (x(1)%a)
  integer            :: z(1,2) = (x(1)%a) 
  integer            :: y(1,2), v(1,2), w(1,2)
  v = (u   %a)
  w =  x(1)%a
  y = (x(1)%a)
  print *, v, w, y, z, p
end
