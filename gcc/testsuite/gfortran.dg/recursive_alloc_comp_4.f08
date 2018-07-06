! { dg-do run }
!
! Tests functionality of recursive allocatable derived types.
! Here the recursive components are arrays, unlike the first three testcases.
! Notice that array components are fiendishly difficult to use :-(
!
module m
  type :: recurses
    type(recurses), allocatable :: c(:)
    integer, allocatable :: ia
  end type
end module

  use m
  type(recurses), allocatable, target :: a, d(:)
  type(recurses), pointer :: b1

  integer :: total = 0

! Check chained allocation.
  allocate(a)
  a%ia = 1
  allocate (a%c(2))
  b1 => a%c(1)
  b1%ia = 2

! Check move_alloc.
  allocate (d(2))
  d(1)%ia = 3
  d(2)%ia = 4
  b1 => d(2)
  allocate (b1%c(1))
  b1  => b1%c(1)
  b1%ia = 5
  call move_alloc (d, a%c(2)%c)

  if (a%ia .ne. 1) STOP 1
  if (a%c(1)%ia .ne. 2) STOP 2
  if (a%c(2)%c(1)%ia .ne. 3) STOP 3
  if (a%c(2)%c(2)%ia .ne. 4) STOP 4
  if (a%c(2)%c(2)%c(1)%ia .ne. 5) STOP 5

  if (allocated (a)) deallocate (a)
  if (allocated (d)) deallocate (d)

end
