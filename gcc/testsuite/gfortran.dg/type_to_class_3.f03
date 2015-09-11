! { dg-do run }
!
! Test the fix for the array version of PR64757.
!
! Based on by Michael Lee Rilee  <mike@rilee.net>
!
  type :: Test
    integer :: i
  end type

  type :: TestReference
     class(Test), allocatable :: test(:)
  end type

  type(TestReference) :: testList
  type(test), allocatable :: x(:)

  testList = TestReference([Test(99), Test(199)])  ! Gave: The rank of the element in the
                                                   ! structure constructor at (1) does not
                                                   ! match that of the component (1/0)
! allocate (testList%test(2), source = [Test(99), Test(199)]) ! Works, of course

  x = testList%test

  select type (y => testList%test)    ! Check vptr set
    type is (Test)
      if (any(x%i .ne. y%i)) call abort
    class default
      call abort
  end select
end


