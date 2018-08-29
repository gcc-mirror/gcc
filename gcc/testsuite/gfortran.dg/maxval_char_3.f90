! { dg-do run }
program main
  implicit none
  integer, parameter :: n=5
  character(len=6), dimension(n,n) :: a
  integer, dimension(n,n) :: v
  character(len=6), dimension(n) :: r1, r2
  character(len=6), dimension(:,:), allocatable :: a_alloc
  integer, dimension(:,:), allocatable :: v_alloc
  character(len=6), parameter :: zero = achar(0) // achar(0) // achar(0) // achar(0) // achar(0) // achar(0)
  integer :: i
  character(len=6),dimension(1) :: ret
  logical, dimension(n,n) :: mask
  logical :: smask

  v = reshape([(i*i+200-17*i,i=1,n*n)],shape(v))
  write (unit=a,fmt='(I6.6)') (i*i+200-17*i,i=1,n*n)

  r1 =  maxval(a,dim=1)
  write (unit=r2,fmt='(I6.6)') maxval(v,dim=1)
  if (any (r1 /= r2)) STOP 1
  r1 = 'x'
  write (unit=r1,fmt='(I6.6)') maxval(v,dim=1)
  if (any (r1 /= r2)) STOP 2

  r1 = 'y'
  r1 =  maxval(a,dim=2)
  write (unit=r2,fmt='(I6.6)') maxval(v,dim=2)
  if (any (r1 /= r2)) STOP 3
  r1 = 'z'
  write (unit=r1,fmt='(I6.6)') maxval(v,dim=2)
  if (any (r1 /= r2)) STOP 4

  allocate (a_alloc(0,1), v_alloc(0,1))
  ret = 'what'
  ret = maxval(a_alloc,dim=1)
  if (ret(1) /= zero) STOP 5
  
  r1 = 'qq'
  r1 = maxval(a, dim=1, mask=a>"000200");
  if (any(r1 /= zero .neqv. maxval(v,dim=1, mask=v>200) > 0)) STOP 6
  if (any(maxval(a, dim=1, mask=a>"000200") /= zero .neqv. maxval(v,dim=1, mask=v>200) > 0)) STOP 7

  r1 = 'rr'
  r1 = maxval(a, dim=2, mask=a>"000200");
  if (any(r1 /= zero .neqv. maxval(v,dim=2, mask=v>200) > 0)) STOP 8
  if (any(maxval(a, dim=2, mask=a>"000200") /= zero .neqv. maxval(v,dim=2, mask=v>200) > 0)) STOP 9

  mask = .true.
  forall (i=1:n)
     mask(i,i) = .false.
  end forall

  r1 = 'aa'
  r1 = maxval(a, dim=1, mask=mask)
  write(unit=r2,fmt='(I6.6)') maxval(v,dim=1, mask=mask)
  if (any(r1 /= r2)) STOP 10

  r1 = 'xyz'
  smask = .true.
  r1 = maxval(a, dim=1, mask=smask)
  write (unit=r2,fmt='(I6.6)') maxval(v,dim=1)
  if (any (r1 /= r2)) STOP 11

  smask = .false.
  r1 = 'foobar'
  r1 = maxval(a, dim=1, mask=smask)
  if (any(r1 /= zero)) STOP 12
end program main
