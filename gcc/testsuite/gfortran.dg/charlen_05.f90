! { dg-do compile }
! PR fortran/65173
program p
   type t
      character(*) :: x y  ! { dg-error "error in data declaration" }
   end type
end
! { dg-excess-errors "needs to be a constant specification" } 
