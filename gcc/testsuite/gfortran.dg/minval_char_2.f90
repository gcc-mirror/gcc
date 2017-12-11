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
  integer(kind=4), dimension(5) :: kmin = [-1, -1, -1, -1, -1]
  character(kind=4,len=5) :: all_full
  logical :: smask
  
  all_full = transfer(kmin,all_full)
  write (unit=a,fmt='(I5.5)') (21-i*i+6*i,i=1,n)
  res = minval(a)
  if (res /= 4_'00026') call abort
  do
     call random_number(r)
     v = int(r * 100)
     if (count(v<30) > 1) exit
  end do
  write (unit=b,fmt='(I5.5)') v
  write (unit=res,fmt='(I5.5)') minval(v)
  if (res /= minval(b)) call abort
  smask = .true.
  if (res /= minval(b, smask)) call abort
  smask = .false.
  if (all_full /= minval(b, smask)) call abort

  mask = v < 30
  write (unit=res,fmt='(I5.5)') minval(v,mask)
  if (res /= minval(b, mask)) call abort
  mask = .false.
  if (minval(b, mask) /= all_full) call abort
  allocate (empty(0:3,0))
  res = minval(empty)
  if (res /= all_full) call abort
end program main
