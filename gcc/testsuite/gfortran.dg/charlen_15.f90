! { dg-do run }
! PR fortran/78350 
module m
   type t
      character(2) :: c(1) = [character(3) :: 'abc']
   end type
   type(t) :: x
end
program foo
  use m
  if (trim(x%c(1)) /= 'ab') STOP 1
end program foo
