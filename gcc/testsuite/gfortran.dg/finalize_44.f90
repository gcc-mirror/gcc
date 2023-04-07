! { dg-do run }
!
! Test the fix for all three variants of PR82996, which used to
! segfault in the original testcase and ICE in the testcases of
! comments 1 and 2.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module mod0
  integer :: final_count_foo = 0
  integer :: final_count_bar = 0
end module mod0
!
! This is the original testcase, with a final routine 'foo' but
! but not in the container type 'bar1'.
!
module mod1
  use mod0
  private foo, foo_destroy
  type foo
    integer, pointer :: f(:) => null()
  contains
    final :: foo_destroy
  end type
  type bar1
    type(foo) :: b(2)
  end type
contains
  impure elemental subroutine foo_destroy(this)
    type(foo), intent(inout) :: this
    final_count_foo = final_count_foo + 1
    if (associated(this%f)) deallocate(this%f)
  end subroutine
end module mod1
!
! Comment 1 was the same as original, except that the
! 'foo' finalizer is elemental and a 'bar' finalizer is added..
!
module mod2
  use mod0
  private foo, foo_destroy, bar_destroy
  type foo
    integer, pointer :: f(:) => null()
  contains
    final :: foo_destroy
  end type
  type bar2
    type(foo) :: b(2)
  contains
    final :: bar_destroy
  end type
contains
  impure elemental subroutine foo_destroy(this)
    type(foo), intent(inout) :: this
    final_count_foo = final_count_foo + 1
    if (associated(this%f)) deallocate(this%f)
  end subroutine
  subroutine bar_destroy(this)
    type(bar2), intent(inout) :: this
    final_count_bar = final_count_bar + 1
    call foo_destroy(this%b)
  end subroutine
end module mod2
!
! Comment 2 was the same as comment 1, except that the 'foo'
! finalizer is no longer elemental.
!
module mod3
  use mod0
  private foo, foo_destroy, bar_destroy
  type foo
    integer, pointer :: f(:) => null()
  contains
    final :: foo_destroy
  end type
  type bar3
    type(foo) :: b(2)
  contains
    final :: bar_destroy
  end type
contains
  subroutine foo_destroy(this)
    type(foo), intent(inout) :: this
    final_count_foo = final_count_foo + 1
    if (associated(this%f)) deallocate(this%f)
  end subroutine
  subroutine bar_destroy(this)
    type(bar3), intent(inout) :: this
    final_count_bar = final_count_bar + 1
    do j = 1, size(this%b)
      call foo_destroy(this%b(j))
    end do
  end subroutine
end module mod3

program main
  use mod0
  use mod1
  use mod2
  use mod3
  type(bar1) :: x
  type(bar2) :: y
  type(bar3) :: z
  call sub1(x)
  if (final_count_foo /= 2) stop 1
  if (final_count_bar /= 0) stop 2
  call sub2(y)
  if (final_count_foo /= 6) stop 3
  if (final_count_bar /= 1) stop 4
  call sub3(z)
  if (final_count_foo /= 8) stop 5
  if (final_count_bar /= 2) stop 6
contains
  subroutine sub1(x)
    type(bar1), intent(out) :: x
  end subroutine
  subroutine sub2(x)
    type(bar2), intent(out) :: x
  end subroutine
  subroutine sub3(x)
    type(bar3), intent(out) :: x
  end subroutine
end program
