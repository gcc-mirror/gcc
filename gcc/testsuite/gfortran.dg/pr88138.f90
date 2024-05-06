! { dg-do compile }
program p
   implicit none
   type t
      character :: c = 'c'
   end type
   type(t), parameter :: x  = 1.e1  ! { dg-error "Incompatible initialization between a" }
   print *, 'a' // x%c
end
! { dg-prune-output "has no IMPLICIT type" }
