! { dg-do compile }
!
! PR fortran/47034
!
! Contributed by James Van Buskirk
!
subroutine james
   use iso_c_binding
   type(C_PTR), parameter :: p1 = &
   transfer(32512_C_INTPTR_T,C_NULL_PTR)
   integer(C_INTPTR_T), parameter :: n1 = transfer(p1,0_C_INTPTR_T)
end
