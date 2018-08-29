! { dg-do run }
!
! Test the fix for PR64757.
!
! Contributed by Michael Lee Rilee  <mike@rilee.net>
!
  type :: Test
    integer :: i
  end type

  type :: TestReference
     class(Test), allocatable :: test
  end type

  type(TestReference) :: testList
  type(test) :: x

  testList = TestReference(Test(99))  ! ICE in fold_convert_loc was here

  x = testList%test

  select type (y => testList%test)    ! Check vptr set
    type is (Test)
      if (x%i .ne. y%i) STOP 1
    class default
      STOP 2
  end select
end


