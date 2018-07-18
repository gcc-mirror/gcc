! { dg-do compile }
! PR fortran/65173
program p
   type t
   end type
   type, extends(t) :: t2
      character x =  ! { dg-error "error in data declaration" }
   end type
end
