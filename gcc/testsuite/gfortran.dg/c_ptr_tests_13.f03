! { dg-do compile }
! Ensure that the user cannot call the structure constructor for one of 
! the iso_c_binding derived types.
!
! PR fortran/33760
!
program main
   use ISO_C_BINDING
   implicit none
   integer(C_INTPTR_T) p
   type(C_PTR) cptr
   p = 0
   cptr = C_PTR(p+1) ! { dg-error "is a PRIVATE component of 'c_ptr'" }
   cptr = C_PTR(1) ! { dg-error "is a PRIVATE component of 'c_ptr'" }
end program main
