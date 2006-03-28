! { dg-do compile }
! Tests the fix for PR26779, where an error would occur because
! init was detected to be public with a private type dummy argument.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module test
  public sub
  type, private :: t
    integer :: i
  end type t
contains
  subroutine sub (arg)
    integer arg
    type(t) :: root
    call init(root, arg)
  contains
    subroutine init(ir, i)
      integer i
      type(t) :: ir
      ir%i = i
    end subroutine init
  end subroutine sub
end module test