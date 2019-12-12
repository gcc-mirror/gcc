! { dg-do compile }
! PR fortran/91716
! Code contributed by Gerhard Steinmetz
module m
   type t
      character :: c(2) = [character(-1) :: 'a', 'b']
   end type
end
