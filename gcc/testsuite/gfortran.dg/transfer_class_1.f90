! { dg-do compile }
! { dg-options "-Wsurprising" }
!
! PR 54917: [4.7/4.8 Regression] [OOP] TRANSFER on polymorphic variable causes ICE
!
! Contributed by Sean Santos <quantheory@gmail.com>

subroutine test_routine1(arg)
  implicit none
  type test_type
    integer :: test_comp
  end type
  class(test_type) :: arg
  integer :: i
  i = transfer(arg, 1)
end subroutine
