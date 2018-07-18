! { dg-do compile }
! PR fortran/65173
program p
   type t
      character, allocatable :: z1(:), z1(:)    ! { dg-error "already declared at" }
   end type
end
