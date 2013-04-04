! { dg-do compile }
! { dg-options "-std=f2003" }

subroutine foo(a,b,c,d)
   use iso_c_binding, only: c_loc, c_ptr
   implicit none
   real, intent(in), target :: a(:)
   real, intent(in), target :: b(5)
   real, intent(in), target :: c(*)
   real, intent(in), target, allocatable :: d(:)
   type(c_ptr) :: ptr
   ptr = C_LOC(b)
   ptr = C_LOC(c)
   ptr = C_LOC(d)
   ptr = C_LOC(a) ! { dg-error "Fortran 2008: Array of interoperable type at .1. to C_LOC which is nonallocatable and neither assumed size nor explicit size" }
end subroutine foo
