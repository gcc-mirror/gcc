! { dg-do run }
!
! PR fortran/66929
! Check that the specific FIRST symbol is used for the call to FOO,
! so that the J argument is not assumed to be present

module m
  interface foo
    module procedure first
  end interface foo
contains
  elemental function bar(j) result(r)
    integer, intent(in), optional :: j
    integer :: r, s(2)
    ! We used to have NULL dereference here, in case of a missing J argument
    s = foo(j, [3, 7])
    r = sum(s)
  end function bar
  elemental function first(i, j) result(r)
    integer, intent(in), optional :: i
    integer, intent(in) :: j
    integer :: r
    if (present(i)) then
      r = i
    else
      r = -5
    end if
  end function first
end module m
program p
  use m
  integer :: i
  i = bar()
  if (i /= -10) STOP 1
end program p
