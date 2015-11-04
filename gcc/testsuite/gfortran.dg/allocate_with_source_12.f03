! { dg-do run }
!
! Checks the fix for PR67171, where the second ALLOCATE with and array section
! SOURCE produced a zero index based temporary, which threw the assignment.
!
! Contributed by Anton Shterenlikht  <mexas@bristol.ac.uk>
!
program z
  implicit none
  integer, parameter :: DIM1_SIZE = 10
  real, allocatable :: d(:,:), tmp(:,:)
  integer :: i, errstat

  allocate (d(DIM1_SIZE, 2), source = 0.0, stat=errstat )

  d(:,1) = [( real (i), i=1,DIM1_SIZE)]
  d(:,2) = [( real(2*i), i=1,DIM1_SIZE)]
!  write (*,*) d(1, :)

  call move_alloc (from = d, to = tmp)
!  write (*,*) tmp( 1, :)

  allocate (d(DIM1_SIZE / 2, 2), source = tmp(1 : DIM1_SIZE / 2, :) , stat=errstat)
  if (any (d .ne. tmp(1:DIM1_SIZE/2,:))) call abort
  deallocate (d)

  allocate (d(DIM1_SIZE / 2, 2), source = foo (tmp(1 : DIM1_SIZE / 2, :)) , stat=errstat)
  if (any (d .ne. tmp(1 : DIM1_SIZE / 2, :))) call abort

  deallocate (tmp , d)

contains
  function foo (arg) result (res)
    real :: arg(:,:)
    real :: res(size (arg, 1), size (arg, 2))
    res = arg
  end function
end program z
