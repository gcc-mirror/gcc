! { dg-do run }
! PR 49802
! VALUE was rejected outright for array dummy arguments ("VALUE
! attribute conflicts with DIMENSION attribute"), even though F2018
! C862 only prohibits VALUE for assumed-size arrays (and coarrays).
! Verify that assumed-shape, explicit-shape, and
! non-contiguous array actuals are passed by VALUE correctly: the
! callee gets a private copy, and modifications do not propagate back
! to the actual argument, including for PARAMETER actuals.

program test
  implicit none
  integer, parameter :: p(5) = [1,2,3,4,5]
  character(len=*), parameter :: c1(2) = [ "abc", "def" ]
  integer :: a(10), i

  a = [(i, i=1,10)]

  call sub_int_assumed_shape (p)
  if (any (p /= [1,2,3,4,5])) stop 1

  call sub_int_noncontig (a(1:10:2))
  if (any (a /= [(i, i=1,10)])) stop 2

  call sub_int_explicit (p)
  if (any (p /= [1,2,3,4,5])) stop 3

  call sub_opt_array (p)
  call sub_opt_array ()

  call sub_char_assumed_shape (c1)
  if (c1(1) /= "abc") stop 4

contains

  subroutine sub_int_assumed_shape (x)
    integer, value :: x(:)
    x = x + 100
    if (any (x /= [101,102,103,104,105])) stop 11
  end subroutine

  subroutine sub_int_noncontig (x)
    integer, value :: x(:)
    x = -1
    if (any (x /= -1)) stop 12
  end subroutine

  subroutine sub_int_explicit (x)
    integer, value :: x(5)
    x(1) = -99
    if (x(1) /= -99) stop 13
  end subroutine

  subroutine sub_opt_array (x)
    integer, value, optional :: x(:)
    if (present (x)) then
      if (any (x /= [1,2,3,4,5])) stop 14
      x = -1
    end if
  end subroutine

  subroutine sub_char_assumed_shape (x)
    character(len=*), value :: x(:)
    if (len (x) /= 3) stop 15
    x(1)(1:1) = "1"
    if (x(1)(1:1) /= "1") stop 16
  end subroutine

end program test
