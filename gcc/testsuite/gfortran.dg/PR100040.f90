! { dg-do run }
!
! Test the fix for PR100040
!

program foo_p
  implicit none

  integer, parameter :: n = 11

  type :: foo_t
    integer :: i
  end type foo_t
  
  type(foo_t), parameter :: a = foo_t(n)
  
  class(foo_t), allocatable :: pout

  call foo_s(pout)
  if(.not.allocated(pout)) stop 1
  if(pout%i/=n) stop 2

contains

  subroutine foo_s(that)
    class(foo_t), allocatable, intent(out) :: that(..)

    select rank(that)
    rank(0)
      that = a
    rank default
      stop 3
    end select
  end subroutine foo_s

end program foo_p
