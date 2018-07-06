! { dg-do run }
! { dg-options "-fcray-pointer" }
!
use iso_c_binding
real target(10)
real pointee(10)
pointer (ipt, pointee)
integer(c_intptr_t) :: int_cptr
real :: x
if (c_sizeof(ipt) /= c_sizeof(int_cptr)) STOP 1
if (c_sizeof(pointee) /= c_sizeof(x)*10) STOP 2
end
