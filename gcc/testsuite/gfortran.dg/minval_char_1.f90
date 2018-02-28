! { dg-do run }
program main
  implicit none
  integer, parameter :: n=5, m=3
  character(len=5), dimension(n) :: a
  character(len=5), dimension(n,m) :: b
  character(len=5) :: res
  integer, dimension(n,m) :: v
  real, dimension(n,m) :: r
  integer :: i,j
  logical, dimension(n,m) :: mask
  character(len=5), dimension(:,:), allocatable :: empty
  character(len=5) , parameter :: all_full = achar(255) // achar(255) // achar(255) // achar(255) // achar(255)
  logical :: smask
  
  write (unit=a,fmt='(I5.5)') (21-i*i+6*i,i=1,n)
  res = minval(a)
  if (res /= '00026') STOP 1
  do
     call random_number(r)
     v = int(r * 100)
     if (count(v < 30) > 1) exit
  end do
  write (unit=b,fmt='(I5.5)') v
  write (unit=res,fmt='(I5.5)') minval(v)
  if (res /= minval(b)) STOP 2
  smask = .true.
  if (res /= minval(b, smask)) STOP 3
  smask = .false.
  if (all_full /= minval(b, smask)) STOP 4

  mask = v < 30
  write (unit=res,fmt='(I5.5)') minval(v,mask)
  if (res /= minval(b, mask)) STOP 5
  mask = .false.
  if (minval(b, mask) /= all_full) STOP 6
  allocate (empty(0:3,0))
  res = minval(empty)
  if (res /= all_full) STOP 7
end program main
