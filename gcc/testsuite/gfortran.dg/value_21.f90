! { dg-do run }
! PR 51284
! An element of a polymorphic array passed to a scalar CLASS dummy with the
! VALUE attribute is not a class container, so it has to be repackaged as
! one before the copy is made.

program prog
  implicit none
  type :: t1
    integer :: c1, c2
  end type
  type, extends(t1) :: t2
    integer :: c3
  end type
  type(t2) :: x(5)
  class(t1), allocatable :: y(:)
  integer :: i
  x = [ (t2(i*i*i, i*i, i), i=1,5) ]
  y = x
  call s(x(3), 1)
  if (x(3)%c1 /= 27) error stop 141
  if (x(3)%c2 /= 9) error stop 142
  if (x(3)%c3 /= 3) error stop 143
  call s(y(3), 2)
  if (y(3)%c1 /= 27) error stop 241
  if (y(3)%c2 /= 9) error stop 242
  select type(y)
    type is (t2)
      if (y(3)%c3 /= 3) error stop 243
    class default
      error stop 249
  end select
contains
  subroutine s(a, e)
    class(t1), value :: a
    integer, intent(in) :: e
    if (a%c1 /= 27) error stop e * 100 + 1
    if (a%c2 /= 9) error stop e * 100 + 2
    select type (a)
      type is (t2)
        if (a%c3 /= 3) error stop e * 100 + 3
      class default
        error stop 9
    end select
    call ar(a, e)
    if (a%c1 /= 12) error stop e * 100 + 11
    if (a%c2 /= 8) error stop e * 100 + 12
    select type (a)
      type is (t2)
        if (a%c3 /= 4) error stop e * 100 + 13
      class default
        error stop e * 100 + 19
    end select
  end subroutine
  subroutine ar(a, e)
    class(t1) :: a(..)
    integer, intent(in) :: e
    select rank (a)
      rank(0)
        call check(a, e)
      rank default
        error stop e * 100 + 21
    end select
  end subroutine
  subroutine check(a, e)
    class(t1) :: a
    integer, intent(in) :: e
    if (a%c1 /= 27) error stop e * 100 + 31
    a%c1 = 12
    if (a%c2 /= 9) error stop e * 100 + 32
    a%c2 = 8
    select type (a)
      type is (t2)
        if (a%c3 /= 3) error stop e * 100 + 33
        a%c3 = 4
      class default
        error stop e * 100 + 39
    end select
  end subroutine
end program
