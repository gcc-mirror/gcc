!{ dg-do compile }

! Check PR120843 is fixed

program p
  implicit none

  type T
    integer, allocatable :: arr(:,:) [:,:]
  end type

  type(T) :: o
  integer, allocatable :: vec(:)[:,:]
  integer :: c[*]

  c = 7

  allocate(o%arr(4,3)[2,*], source=6)
  allocate(vec(10)[1,*], source=7)

  if (vec(3) * c /= 49) stop 1
  if (o%arr(2,2)* c /= 42) stop 2

end program p
