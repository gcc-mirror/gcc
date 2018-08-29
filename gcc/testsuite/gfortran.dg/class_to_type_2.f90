! { dg-do run }
!
! PR fortran/51514
!
! Check that passing a CLASS to a TYPE works
!
! Based on a test case of Reinhold Bader.
!

module mod_subpr
  implicit none

  type :: foo
    integer :: i = 2
  end type

  type, extends(foo) :: foo_1
    real :: r(2)
  end type

contains

  subroutine subpr (x)
    type(foo) :: x
    x%i = 3
  end subroutine

  elemental subroutine subpr_elem (x)
    type(foo), intent(inout):: x
    x%i = 3
  end subroutine

  subroutine subpr_array (x)
    type(foo), intent(inout):: x(:)
    x(:)%i = 3
  end subroutine

  subroutine subpr2 (x)
    type(foo) :: x
    if (x%i /= 55) STOP 1
  end subroutine

  subroutine subpr2_array (x)
    type(foo) :: x(:)
    if (any(x(:)%i /= 55)) STOP 2
  end subroutine

  function f ()
    class(foo), allocatable :: f
    allocate (f)
    f%i = 55
  end function f

  function g () result(res)
    class(foo), allocatable :: res(:)
    allocate (res(3))
    res(:)%i = 55
  end function g
end module

program prog
  use mod_subpr
  implicit none
  class(foo), allocatable :: xx, yy(:)

  allocate (foo_1 :: xx)
  xx%i = 33
  call subpr (xx)
  if (xx%i /= 3) STOP 3

  xx%i = 33
  call subpr_elem (xx)
  if (xx%i /= 3) STOP 4

  call subpr (f ())

  allocate (foo_1 :: yy(2))
  yy(:)%i = 33
  call subpr_elem (yy)
  if (any (yy%i /= 3)) STOP 5

  yy(:)%i = 33
  call subpr_elem (yy(1))
  if (yy(1)%i /= 3) STOP 6

  yy(:)%i = 33
  call subpr_array (yy)
  if (any (yy%i /= 3)) STOP 7

  yy(:)%i = 33
  call subpr_array (yy(1:2))
  if (any (yy(1:2)%i /= 3)) STOP 8

 call subpr2_array (g ())
end program
