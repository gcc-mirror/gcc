! { dg-do compile }
! PR fortran/68318
! Original code submitted by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
!
module m1
   implicit none
contains
   subroutine s1
   entry e
   end
end module

module m2
   use m1         ! { dg-error "(2)" }
   implicit none
contains
   subroutine s2
   entry e        ! { dg-error "is already defined" }
   end
end module
! { dg-prune-output "Cannot change attribute" }
