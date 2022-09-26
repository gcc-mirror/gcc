! { dg-do run }
!
! Test the fix for PR100103
!

program main_p
  implicit none

  integer            :: i
  integer, parameter :: n = 11

  type :: foo_t
    integer :: i
  end type foo_t

  type(foo_t), parameter :: a(*) = [(foo_t(i), i=1,n)]

  type(foo_t),  allocatable :: bar_d(:)
  class(foo_t), allocatable :: bar_p(:)
  class(*),     allocatable :: bar_u(:)


  call foo_d(bar_d)
  if(.not.allocated(bar_d)) stop 1
  if(any(bar_d%i/=a%i)) stop 2
  deallocate(bar_d)
  call foo_p(bar_p)
  if(.not.allocated(bar_p)) stop 3
  if(any(bar_p%i/=a%i)) stop 4
  deallocate(bar_p)
  call foo_u(bar_u)
  if(.not.allocated(bar_u)) stop 5
  select type(bar_u)
  type is(foo_t)
    if(any(bar_u%i/=a%i)) stop 6
  class default
    stop 7
  end select
  deallocate(bar_u)

contains

  subroutine foo_d(that)
    type(foo_t), allocatable, intent(out) :: that(..)

    select rank(that)
    rank(1)
      that = a
    rank default
      stop 8
    end select
  end subroutine foo_d

  subroutine foo_p(that)
    class(foo_t), allocatable, intent(out) :: that(..)

    select rank(that)
    rank(1)
      that = a
    rank default
      stop 9
    end select
  end subroutine foo_p

  subroutine foo_u(that)
    class(*), allocatable, intent(out) :: that(..)

    select rank(that)
    rank(1)
      that = a
    rank default
      stop 10
    end select
  end subroutine foo_u

end program main_p
