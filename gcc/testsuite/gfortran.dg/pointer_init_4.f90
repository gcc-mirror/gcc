! { dg-do run }
!
! PR 45290: [F08] pointer initialization
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

implicit none

contains

  integer function f1()
    f1 = 42
  end function

  integer function f2()
    f2 = 43
  end function

end module


program test_ptr_init

use m
implicit none

procedure(f1), pointer :: pp => f1

type :: t
  procedure(f2), pointer, nopass :: ppc => f2
end type

type (t) :: u

if (pp()/=42) call abort()
if (u%ppc()/=43) call abort()

end
