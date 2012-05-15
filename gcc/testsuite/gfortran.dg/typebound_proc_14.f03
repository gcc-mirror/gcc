! { dg-do compile }
!
! PR 40117: [OOP][F2008] Type-bound procedure: allow list after PROCEDURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

implicit none

type :: t
contains
  procedure :: foo, bar, baz
end type

contains

  subroutine foo (this)
    class(t) :: this
  end subroutine

  real function bar (this)
    class(t) :: this
  end function

  subroutine baz (this, par)
    class(t) :: this
    integer :: par
  end subroutine

end
