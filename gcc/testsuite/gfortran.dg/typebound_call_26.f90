! { dg-do compile }
!
! PR 64244: [4.8/4.9/5 Regression] ICE at class.c:236 when using non_overridable
!
! Contributed by Ondřej Čertík <ondrej.certik@gmail.com>

module m
  implicit none

  type :: A
  contains
    generic :: f => g
    procedure, non_overridable :: g
  end type

contains

  subroutine g(this)
    class(A), intent(in) :: this
  end subroutine

end module


program test_non_overridable
  use m, only: A
  implicit none
  class(A), allocatable :: h
  call h%f()
end
