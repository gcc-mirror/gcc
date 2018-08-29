! { dg-do compile }
! { dg-options "-O0" }
! PR fortran/68318
! Original code submitted by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
!
module m
   implicit none
contains
   subroutine s1
   entry e        ! { dg-error "(2)" }
   end
   subroutine s2
   entry e        ! { dg-error "is already defined" }
   end
end module
! { dg-prune-output "Duplicate ENTRY attribute specified" }

