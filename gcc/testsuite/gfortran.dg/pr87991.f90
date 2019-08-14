! { dg-do compile }
! { dg-options "-w" }
! PR fortran/87991
program p
   type t
      character(:), pointer :: c
   end type
   type(t) :: x
   allocate (character(3) :: x%c)
   data x%c /'abc'/   ! { dg-error "has the pointer attribute" }
end
