! { dg-do run }
! PR fortran/78350 
program p
   type t
      character(2) :: c(1) = [character(3) :: 'abc']
   end type
   type(t) :: x
   if (trim(x%c(1)) /= 'ab') call abort
end
