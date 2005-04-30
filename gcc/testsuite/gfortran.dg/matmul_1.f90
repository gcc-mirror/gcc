!{ dg-do run }
! Test MATMUL for various arguments and results
! (test values checked with GNU octave).
! PR18857 was due to an incorrect assertion that component base==0
! for both input arguments and the result.
! provided by Paul Thomas - pault@gcc.gnu.org

Program matmul_1
  integer, parameter                                :: N = 5
  integer, parameter                                :: T = 4
  integer                                           :: i
  real(kind=T), dimension(:,:), allocatable         :: a, b, c
  real(kind=T), dimension(N,N)                      :: x, y, z

  allocate (a(2*N, N), b(N, N), c(2*N, N))

  do i = 1, 2*N
    a(i, :) = real (i)
  end do
  b = 4.0_T

  do i = 1, N
    x(i, :) = real (i)
  end do
  y = 2.0_T

!                           whole array

  z = 0.0_T
  z = matmul (x, y)
  if (sum (z) /= 750.0_T) call abort ()

!                           array sections

  c = 0.0_T
  c = matmul (a(7:9,3:N), b(3:N,3:4))
  if (sum (c) /= 576.0_T) call abort ()

!                           uses a temp

  c = 0.0_T
  c = matmul (a, b + x)
  if (sum (c) /= 9625.0_T) call abort ()

!                           returns to a temp

  c = 0.0_T
  c = a + matmul (a, b)
  if (sum (c) /= 5775.0_T) call abort ()

  deallocate (a, b, c)

end program matmul_1
