! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR 40117: [OOP][F2008] Type-bound procedure: allow list after PROCEDURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org> 

module m

implicit none

type :: t
contains
  procedure :: foo
  procedure :: bar, baz  ! { dg-error "PROCEDURE list" }
end type

contains

  subroutine foo (this)
    class(t) :: this
  end subroutine

  subroutine bar (this)
    class(t) :: this
  end subroutine

  subroutine baz (this)
    class(t) :: this
  end subroutine

end
