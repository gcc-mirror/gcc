! { dg-do compile }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module inteface_assignment_6

  type :: t
  end type

  ! this was rejected as ambiguous, but is valid in F08
  interface assignment(=)
    procedure testAlloc
    procedure testPtr
  end interface

contains

  subroutine testAlloc(obj, val)
    type(t), allocatable, intent(out) :: obj
    integer, intent(in) :: val
  end subroutine

  subroutine testPtr(obj, val)
    type(t), pointer, intent(out) :: obj
    integer, intent(in) :: val
  end subroutine

end
