! { dg-do compile }
!
! Test the fix for PR93701.
!
! Contributed by Simon Brass  <simon.brass@desy.de>
!
module test
  implicit none

  integer, parameter :: N_STATE = 1, &
       TEST_STATE = 1

  type :: test_t
     integer, dimension(:), allocatable :: state
  end type test_t

contains

  subroutine test_allocate (obj)
    class(test_t), intent(out) :: obj
    allocate (obj%state(N_STATE))
  end subroutine test_allocate


  subroutine test_alter_state2 (obj, a)
    class(test_t), intent(inout) :: obj
    integer, intent(in) :: a
    associate (state => obj%state(TEST_STATES)) ! { dg-error "no IMPLICIT type" }
      state = a  ! { dg-error "cannot be used in a variable definition context" }
!      state(TEST_STATE) = a
    end associate
  end subroutine test_alter_state2

end module test

