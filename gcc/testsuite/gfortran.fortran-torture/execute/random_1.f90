! PR15619
! Check that random_seed works as expected.
! Does not check the quality of random numbers, hence should never fail.
program test_random
  implicit none
  integer, allocatable :: seed(:)
  real, dimension(10) :: a, b
  integer n; 

  call random_seed (size=n)
  allocate (seed(n))
  
  ! Exercise the generator a bit.
  call random_number (a)

  ! Remeber the seed and get 10 more.
  call random_seed (get=seed)
  call random_number (a)

  ! Get the same 10 numbers in two blocks, remebering the seed in the middle
  call random_seed (put=seed)
  call random_number (b(1:5))
  call random_seed(get=seed)
  call random_number (b(6:10))
  if (any (a .ne. b)) call abort

  ! Get the last 5 numbers again.
  call random_seed (put=seed)
  call random_number (b(6:10))
  if (any (a .ne. b)) call abort
end program


