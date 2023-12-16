! { dg-do run }
!
! Test the fix for PR112834 in which class array function selectors caused
! problems for both ASSOCIATE and SELECT_TYPE.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module m
  implicit none
  type t
    integer :: i = 0
  end type t
  integer :: i = 0
  type(t), parameter :: test_array (2) = [t(42),t(84)], &
                        test_scalar = t(99)
end module m
module class_selectors
  use m
  implicit none
  private
  public foo2
contains
  function bar3() result(res)
    class(t), allocatable :: res(:)
    allocate (res, source = test_array)
  end

  subroutine foo2()
    associate (var1 => bar3())
      if (any (var1%i .ne. test_array%i)) stop 1
      if (var1(2)%i .ne. test_array(2)%i) stop 2
      associate (zzz3 => var1%i)
        if (any (zzz3 .ne. test_array%i)) stop 3
        if (zzz3(2) .ne. test_array(2)%i) stop 4
      end associate
      select type (x => var1)
        type is (t)
          if (any (x%i .ne. test_array%i)) stop 5
          if (x(2)%i .ne. test_array(2)%i) stop 6
        class default
          stop 7
      end select
    end associate

    select type (y => bar3 ())
      type is (t)
        if (any (y%i .ne. test_array%i)) stop 8
        if (y(2)%i .ne. test_array(2)%i) stop 9
       class default
        stop 10
    end select
  end subroutine foo2
end module class_selectors

  use class_selectors
  call foo2
end
