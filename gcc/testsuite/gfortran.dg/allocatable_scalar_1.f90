! { dg-do run }
!
! PR 40996: [F03] ALLOCATABLE scalars
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none
real, allocatable :: scalar

allocate(scalar)
scalar = exp(1.)
print *,scalar
if (.not. allocated(scalar)) STOP 1
deallocate(scalar)
if (allocated(scalar)) STOP 2

end

