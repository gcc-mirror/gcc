! { dg-do run }
program main
  implicit none
  real, allocatable :: a(:), b(:,:)
  integer :: n,m
  character (len=2) :: one, two

  one = ' 1'
  two = ' 2'

  allocate (a(1:-1))
  if (size(a) /= 0) call abort
  deallocate (a)

  allocate (b(1:-1,0:10))
  if (size(b) /= 0) call abort
  deallocate (b)

  ! Use variables for array bounds.  The internal reads
  ! are there to hide fact that these are actually constant.

  read (unit=one, fmt='(I2)') n
  allocate (a(n:-1))
  if (size(a) /= 0) call abort
  deallocate (a)

  read (unit=two, fmt='(I2)') m
  allocate (b(1:3, m:0))
  if (size(b) /= 0) call abort
  deallocate (b)
end program main
