! { dg-do compile }
! Test the fix for PR93678 in which the charlen for the 'unpackbytes'
! vtable field was incomplete and caused the ICE as indicated.
! Contributed by Luis Kornblueh  <mail.luis@web.de>
!
! The testcase was reduced by various gfortran regulars.
module mo_a
  implicit none
  type t_b
    integer :: i
  contains
    procedure :: unpackbytes => b_unpackbytes
  end type t_b
contains
  function b_unpackbytes (me) result (res)
    class(t_b), intent(inout) :: me
    character                 :: res(1)
    res = char (me%i)
  end function b_unpackbytes
  subroutine b_unpackint (me, c)
    class(t_b), intent(inout) :: me
    character, intent(in) :: c
!   print *, b_unpackbytes (me) ! ok
    if (any (me% unpackbytes () .ne. c)) stop 1 ! ICEd here
  end subroutine b_unpackint
end module mo_a

  use mo_a
  class(t_b), allocatable :: z
  allocate (z, source = t_b(97))
  call b_unpackint (z, "a")
end
