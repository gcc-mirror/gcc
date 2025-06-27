!{ dg-do compile }

! Check PR120843 is fixed

program p
  implicit none

  integer, allocatable :: arr(:,:) [:,:]
  integer :: c[*]

  c = 7

  allocate(arr(4,3)[2,*], source=6)

  if (arr(2,2)* c /= 42) stop 1

end program p
