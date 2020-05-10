! { dg-do compile }
! PR 93499 - this used to ICE. Original test case by Gerhard Steinmetz.

program p
  integer :: a((0.)/0)  ! { dg-error "Division by zero" }
  type t(n)
     integer, len :: n
  end type t
  type(t((0)/0))  :: x  ! { dg-error "Division by zero" }
end
