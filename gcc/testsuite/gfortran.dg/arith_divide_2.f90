! { dg-do compile }
! PR 92961 - this used to ICE. Original test case by Gerhard Steinmetz.
program p
   integer :: a((0)/0)    ! { dg-error "Division by zero" }
   integer :: b(0/(0))    ! { dg-error "Division by zero" }
   integer :: c((0)/(0))  ! { dg-error "Division by zero" }
   integer :: d(0/0)      ! { dg-error "Division by zero" }
   integer :: x = ubound(a,1) ! { dg-error "must be an array" }
end
