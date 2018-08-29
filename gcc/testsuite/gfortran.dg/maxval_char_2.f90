! { dg-do run }
program main
  implicit none
  integer, parameter :: n=5, m=3
  character(kind=4,len=5), dimension(n) :: a
  character(kind=4,len=5), dimension(n,m) :: b
  character(kind=4,len=5) :: res
  integer, dimension(n,m) :: v
  real, dimension(n,m) :: r
  integer :: i,j
  logical, dimension(n,m) :: mask
  character(kind=4,len=5), dimension(:,:), allocatable :: empty
  character(kind=4,len=5) , parameter :: all_zero = achar(0) // achar(0) // achar(0) // achar(0) // achar(0)
  logical :: smask
  
  write (unit=a,fmt='(I5.5)') (21-i*i+6*i,i=1,n)
  res = maxval(a)
  if (res /= 4_'00030') STOP 1
  do
     call random_number(r)
     v = int(r * 100)
     if (count(v > 20) > 1) exit
  end do
  write (unit=b,fmt='(I5.5)') v
  write (unit=res,fmt='(I5.5)') maxval(v)
  if (res /= maxval(b)) STOP 2
  smask = .true.
  if (res /= maxval(b, smask)) STOP 3
  smask = .false.
  if (all_zero /= maxval(b, smask)) STOP 4

  mask = v > 20
  write (unit=res,fmt='(I5.5)') maxval(v,mask)
  if (res /= maxval(b, mask)) STOP 5
  mask = .false.
  if (maxval(b, mask) /= all_zero) STOP 6
  allocate (empty(0:3,0))
  res = maxval(empty)
  if (res /= all_zero) STOP 7
end program main
