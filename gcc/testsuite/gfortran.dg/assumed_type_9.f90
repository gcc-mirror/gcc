! { dg-do run }
!
! Test the fix for PR85742 in which the descriptors, passed to alsize,
! for 'a' and 'b' had the wrong element length.
!
! Contributed by Cesar Philippidis  <cesar@gcc.gnu.org>
!
program main
  implicit none
  integer, allocatable :: a
  real, pointer :: b
  integer, allocatable :: am(:,:)
  real, pointer :: bm(:,:)

  allocate (a)
  allocate (b)
  allocate (am(3,3))
  allocate (bm(4,4))

  if (sizeof (a) /= alsize (a)) stop 1
  if (sizeof (b) /= alsize (b)) stop 2
  if (sizeof (am) /= alsize (am)) stop 3
  if (sizeof (bm) /= alsize (bm)) stop 4

  deallocate (b)
  deallocate (bm)
contains
  function alsize (a)
    integer alsize
    type (*), dimension (..), contiguous :: a
    alsize = sizeof(a)
  end function
end program main

