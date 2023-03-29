! { dg-do run }
!
! Test the fix for PR100029
!

program foo_p
  implicit none

  type :: foo_t
  end type foo_t
  
  class(foo_t), allocatable :: pout

  call foo_s(pout)

contains

  subroutine foo_s(that)
    class(foo_t), allocatable, intent(out) :: that(..)
  end subroutine foo_s

end program foo_p
