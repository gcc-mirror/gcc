! { dg-do compile }
! Tests the fix for PR30283 in which the type of the result
! of bar was getting lost

! Contributed by Harald Anlauf <anlauf@gmx.de>

module gfcbug50
  implicit none
contains

  subroutine foo (n, y)
    integer, intent(in)         :: n
    integer, dimension(bar (n)) :: y
    ! Array bound is specification expression, which is allowed (F2003, sect.7.1.6)
  end subroutine foo

  pure function bar (n) result (l)
    integer, intent(in) :: n
    integer             :: l
    l = n
  end function bar

end module gfcbug50

! { dg-final { cleanup-modules "gfcbug50" } }
