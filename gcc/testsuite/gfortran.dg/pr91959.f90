! { dg-do compile }
! PR fortran/91959
! Code contributed by Gerhard Steinmetz
program p
   implicit none
   integer :: %a  ! { dg-error "Invalid character" }
   a = 1          ! { dg-error "has no IMPLICIT type" }
   print *, a 
end
