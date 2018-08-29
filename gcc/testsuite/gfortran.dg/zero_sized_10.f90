! { dg-do compile }
! { PR 85111 - this used to ICE. }
! Original test case by Gernhard Steinmetz.
program p
   integer, parameter :: a(2,0) = reshape([1,2,3,4], shape(a))
   character, parameter :: ac(2,0) = reshape(['a','b','c','d'], shape(ac))
   integer, parameter :: b(2) = maxloc(a, dim=1) ! { dg-error "Different shape" }
   integer, parameter :: c(2) = minloc(a, dim=1) ! { dg-error "Different shape" }
   character, parameter :: d(2) = maxval(ac, dim=1) ! { dg-error "Different shape" }
 end program p
