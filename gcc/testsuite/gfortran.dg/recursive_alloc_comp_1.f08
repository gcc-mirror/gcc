! { dg-do run }
!
! Tests functionality of recursive allocatable derived types.
!
  type :: recurses
    type(recurses), allocatable :: c
    integer, allocatable :: ia
  end type

  type(recurses), allocatable, target :: a, d
  type(recurses), pointer :: b

  integer :: total = 0

! Check chained allocation.
  allocate(a)
  a%ia = 1
  allocate (a%c)
  a%c%ia = 2

! Check move_alloc.
  allocate (d)
  d%ia = 3
  call move_alloc (d, a%c%c)

  if (a%ia .ne. 1)  call abort
  if (a%c%ia .ne. 2)  call abort
  if (a%c%c%ia .ne. 3)  call abort

! Check that we can point anywhere in the chain
  b => a%c%c
  if (b%ia .ne. 3) call abort
  b => a%c
  if (b%ia .ne. 2) call abort

! Check that the pointer can be used as if it were an element in the chain.
  if (.not.allocated (b%c)) call abort
  b => a%c%c
  if (.not.allocated (b%c)) allocate (b%c)
  b%c%ia = 4
  if (a%c%c%c%ia .ne. 4) call abort

! A rudimentary iterator.
  b => a
  do while (associated (b))
    total = total + b%ia
    b => b%c
  end do
  if (total .ne. 10) call abort

! Take one element out of the chain.
  call move_alloc (a%c%c, d)
  call move_alloc (d%c, a%c%c)
  if (d%ia .ne. 3) call abort
  deallocate (d)

! Checkcount of remaining chain.
  total = 0
  b => a
  do while (associated (b))
    total = total + b%ia
    b => b%c
  end do
  if (total .ne. 7) call abort

! Deallocate to check that there are no memory leaks.
  deallocate (a%c%c)
  deallocate (a%c)
  deallocate (a)
end
