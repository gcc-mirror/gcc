! { dg-do run }
!
! PR 51284
! The copy of the actual argument for a VALUE dummy of derived type with
! allocatable components, or of CLASS type.  An absent optional actual has
! no copy, a function result or constructor is evaluated and freed once,
! and a CLASS actual for a TYPE dummy and an intrinsic actual for a
! CLASS(*) dummy are copied.  Checked under -fsanitize=address.

module m
  implicit none
  type :: t
    integer, allocatable :: v(:)
  end type
  integer :: ncall = 0
contains
  subroutine st (x)
    type(t), value :: x
    if (x%v(1) /= 1) stop 1
    x%v(1) = 99
  end subroutine
  subroutine st_free (x)
    type(t), value :: x
    if (x%v(1) /= 1) stop 2
    deallocate (x%v)
  end subroutine
  subroutine st_opt (x)
    type(t), value, optional :: x
    if (present (x)) then
      if (x%v(1) /= 1) stop 3
      x%v(1) = 99
    end if
  end subroutine
  subroutine pass_opt (y)
    type(t), optional :: y
    call st_opt (y)
  end subroutine
  subroutine pass_opt_value (y)
    type(t), value, optional :: y
    call st_opt (y)
  end subroutine
  subroutine sc (x)
    class(t), value :: x
    if (x%v(1) /= 1) stop 4
    x%v(1) = 99
  end subroutine
  subroutine su (x)
    class(*), value :: x
    select type (x)
    type is (integer)
      if (x /= 1) stop 6
      x = 99
    type is (character(*))
      if (x /= 'abc') stop 7
      x = 'xyz'
    class default
      stop 8
    end select
  end subroutine
  function f () result (r)
    type(t) :: r
    ncall = ncall + 1
    r%v = [1]
  end function
  function g () result (r)
    class(t), allocatable :: r
    ncall = ncall + 1
    allocate (r, source = t ([1]))
  end function
  subroutine work ()
    type(t) :: a
    type(t), allocatable :: b
    class(t), allocatable :: c, ca(:)
    integer :: i
    character(3) :: ch

    a = t ([1])
    call pass_opt (a)
    if (a%v(1) /= 1) stop 11
    call pass_opt ()
    call pass_opt_value (a)
    if (a%v(1) /= 1) stop 18
    call pass_opt_value ()
    call st_opt (b)

    ncall = 0
    call st (f ())
    call st_free (f ())
    call sc (f ())
    call sc (g ())
    if (ncall /= 4) stop 12
    call st (t ([1]))

    allocate (c, source = t ([1]))
    call st (c)
    if (c%v(1) /= 1) stop 13
    allocate (ca(2), source = t ([1]))
    call st (ca(2))
    if (ca(2)%v(1) /= 1) stop 14

    i = 1
    call su (i)
    if (i /= 1) stop 16
    call su (1)
    ch = 'abc'
    call su (ch)
    if (ch /= 'abc') stop 17
    call su ('abc')
  end subroutine
end module

program p
  use m
  implicit none
  call work ()
end program
