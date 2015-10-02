! { dg-do compile }
! PR fortran/67616
! Original code contributed by Gerhard Steinmetz 
program p
   type t
   end type
   type(t) :: y
   data y /t()/
   block
      type(t) :: x
      data x /t()/      ! Prior to patch, this would ICE.
   end block
end
