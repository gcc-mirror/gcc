! { dg-do run }
!
! PR 40940: CLASS statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

type t
  integer :: comp
  class(t),pointer :: c2
end type

class(t),pointer :: c1

allocate(c1)

c1%comp = 5
c1%c2 => c1

print *,c1%comp

call sub(c1)

if (c1%comp/=5) STOP 1

deallocate(c1)

contains

  subroutine sub (c3)
    class(t) :: c3
    print *,c3%comp
  end subroutine

end

