! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! Check that the random_seed for real(10) exists and that
! real(8) and real(10) random number generators
! return the same sequence of values.
! Mostly copied from random_2.f90
program random_3
  integer,parameter :: k = selected_real_kind (precision (0.0_8) + 1)

  integer, dimension(:), allocatable :: seed
  real(kind=8), dimension(10) :: r8
  real(kind=k), dimension(10) :: r10
  real, parameter :: delta = 1.d-10
  integer n

  ! Run the test only if real(10) is available. With the current
  ! xorshift1024* PRNG the real(16) generator uses two uint64_t values
  ! for every real(16) value generated, and hence the sequences won't
  ! be the same as with real(4,8,10).
  if (k == 10) then
     call random_seed (size=n)
     allocate (seed(n))
     call random_seed (get=seed)
     ! Test both array valued and scalar routines.
     call random_number(r8)
     call random_number (r8(10))

     ! Reset the seed and get the real(8) values.
     call random_seed (put=seed)
     call random_number(r10)
     call random_number (r10(10))

     if (any ((r8 - r10) .gt. delta)) call abort
  end if
end program random_3
