! { dg-do run }
!
! PR 40996: [F03] ALLOCATABLE scalars
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

type :: t
  integer, allocatable :: i
end type

type(t)::x

allocate(x%i)

x%i = 13
print *,x%i
if (.not. allocated(x%i)) STOP 1

deallocate(x%i)

if (allocated(x%i)) STOP 2

end
