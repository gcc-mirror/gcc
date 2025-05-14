! { dg-do run }
!
! Test the fix for PR119948, which used to fail as indicated below with:
! (1) "Error: Bad allocate-object at (1) for a PURE procedure"
! (2) "Error: ‘construct_test2 at (1) is not a variable"
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module test_m
  implicit none

  type test_t
    integer, allocatable :: i
  end type

  interface
    pure module function construct_test(arg) result(test)
      implicit none
      type(test_t) :: test
      type(test_t), intent(in) :: arg
    end function

    pure module function construct_test2(arg)
      implicit none
      type(test_t) construct_test2
      type(test_t), intent(in) :: arg
    end function

    pure module function construct_test_3(arg) result(test)
      implicit none
      type(test_t) :: test
      type(test_t), intent(in) :: arg
    end function

    pure module function construct_test_4(arg)
      implicit none
      type(test_t) :: construct_test_4
      type(test_t), intent(in) :: arg
    end function
  end interface

contains
  module procedure construct_test
    allocate(test%i, source = arg%i) ! Fail #1
  end procedure

  module procedure construct_test2
    allocate(construct_test2%i, source = arg%i)    ! Fail #2
  end procedure
end module

submodule (test_m)test_s
contains
  module procedure construct_test_3
    allocate(test%i, source = arg%i) ! This was OK.
  end procedure

  module procedure construct_test_4
    allocate(construct_test_4%i, source = arg%i) ! This was OK.
  end procedure
end submodule

  use test_m
  type(test_t) :: res, dummy
!
  dummy%i = int (rand () * 1e6)
  res = construct_test (dummy)
  if (res%i /= dummy%i) stop 1
!
  dummy%i = int (rand () * 1e6)
  res = construct_test2 (dummy)
  if (res%i /= dummy%i) stop 2
!
  dummy%i = int (rand () * 1e6)
  res = construct_test_3 (dummy)
  if (res%i /= dummy%i) stop 3

  dummy%i = int (rand () * 1e6)
  res = construct_test_4 (dummy)
  if (res%i /= dummy%i) stop 4

  deallocate (res%i, dummy%i)
end
