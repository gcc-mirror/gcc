! { dg-do run }
! Test findloc with dim argument.

program main
  implicit none
  real, dimension(2,2) :: a, b
  logical, dimension(2,2) :: lo
  a = reshape([1.,2.,3.,4.], shape(a))
  b = reshape([1.,1.,1.,1.], shape(b))

  lo = .true.

  if (any(findloc(b,value=1.,dim=1) /= [1,1])) stop 1
  if (any(findloc(b,value=1.,dim=2) /= [1,1])) stop 2
  if (any(findloc(b,value=1.,dim=1,back=.true.) /= [2,2])) stop 3
  if (any(findloc(b,value=1.,dim=2,back=.true.) /= [2,2])) stop 4
  if (any(findloc(b,value=1.,dim=1,mask=lo) /= [1,1])) stop 5
  
  if (any(findloc(b,value=1.,dim=1,mask=lo,back=.true.) /= [2,2])) stop 6
  if (any(findloc(b,value=1.,dim=1,mask=.not. lo) /= [0,0])) stop 7
  lo(1,1) = .false.
  if (any(findloc(b,value=1.,dim=1,mask=lo) /= [2,1])) stop 8
  if (any(findloc(a,value=1.5,dim=2,back=.true.) /= [0,0])) stop 9
  if (any(findloc(a,value=1,dim=1,mask=lo) /= [0,0])) stop 10
end program main
