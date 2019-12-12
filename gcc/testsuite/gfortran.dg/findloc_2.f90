! { dg-do run }
! Various tests with findloc.
program main
  implicit none
  real, dimension(2,2) :: a, b
  integer, dimension(2,3) :: c
  logical, dimension(2,2) :: lo
  integer, dimension(:), allocatable :: e
  a = reshape([1.,2.,3.,4.], shape(a))
  b = reshape([1.,2.,1.,2.], shape(b))

  lo = .true.

  if (any(findloc(a, 5.) /= [0,0])) stop 1
  if (any(findloc(a, 5., back=.true.) /= [0,0])) stop 2
  if (any(findloc(a, 2.) /= [2,1])) stop 2
  if (any(findloc(a, 2. ,back=.true.) /= [2,1])) stop 3

  if (any(findloc(a,3.,mask=lo) /= [1,2])) stop 4
  if (any(findloc(a,3,mask=.true.) /= [1,2])) stop 5
  lo(1,2) = .false.
  if (any(findloc(a,3.,mask=lo) /= [0,0])) stop 6
  if (any(findloc(b,2.) /= [2,1])) stop 7
  if (any(findloc(b,2.,back=.true.) /= [2,2])) stop 8
  if (any(findloc(b,1.,mask=lo,back=.true.) /= [1,1])) stop 9
  if (any(findloc(b,1.,mask=.false.) /= [0,0])) stop 10

  c = reshape([1,2,2,2,-9,6], shape(c))
  if (any(findloc(c,value=2,dim=1) /= [2,1,0])) stop 11
  if (any(findloc(c,value=2,dim=2) /= [2,1])) stop 12
end program main
