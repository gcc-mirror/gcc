! Check that the real(4) and real(8) random number generators return the same
! sequence of values.
program random_4
  integer, dimension(:), allocatable :: seed
  real(kind=4), dimension(10) :: r4
  real(kind=8), dimension(10) :: r8
  real, parameter :: delta = 0.0001
  integer n

  call random_seed (size=n)
  allocate (seed(n))
  call random_seed (get=seed)
  ! Test both array valued and scalar routines.
  call random_number(r4)
  call random_number (r4(10))

  ! Reset the seed and get the real(8) values.
  call random_seed (put=seed)
  call random_number(r8)
  call random_number (r8(10))

  if (any ((r4 - r8) .gt. delta)) call abort
end program

