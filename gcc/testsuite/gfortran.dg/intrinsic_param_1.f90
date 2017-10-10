! { dg-do compile }
! { dg-additional-options "-std=f95" }
! PR 54633 - this used to be accepted
program main
  integer, parameter :: m = minval((/1/)) ! { dg-error "Transformational function" }
end
