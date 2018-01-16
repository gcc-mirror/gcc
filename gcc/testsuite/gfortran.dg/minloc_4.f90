! { dg-do  run }
! Check that simplification of minloc works
program main
  implicit none
  integer :: d
  real, dimension(2), parameter :: a = [1.0, 0.0]
  character(len=3), dimension(3), parameter :: c = [ "fgh", "asd", "jkl" ]
  integer, parameter :: b = minloc(a,dim=1)
  integer, parameter :: b2 = minloc(a,dim=1,mask=[.false.,.false.])
  integer, parameter :: b3 = minloc(c,dim=1)
  integer, parameter :: b4 = minloc(c,dim=1,mask=[c>"bbb"])
  integer, parameter,dimension(2,2) :: i1 = reshape([4,3,2,5],shape(i1))
  integer, parameter, dimension(2) :: b5 = minloc(i1)
  integer, parameter, dimension(2) :: b6 = minloc(i1,mask=i1>7)
  integer, parameter, dimension(2) :: b7 = minloc(i1, mask=i1>2)
  integer, parameter, dimension(2) :: b8 = minloc(i1, mask=.true.)
  integer, parameter, dimension(2) :: b9 = minloc(i1, mask=.false.)
  integer, parameter, dimension(2,3) :: i2 = &
       reshape([2, -1, -3, 4, -5, 6], shape(i2))
  integer, parameter, dimension(3) :: b10 = minloc(i2, dim=1)
  integer, parameter, dimension(2) :: b11 = minloc(i2, dim=2)
  integer, parameter, dimension(3) :: b12 = minloc(i2,dim=1,mask=i2>3)
  integer, parameter, dimension(2) :: b13 = minloc(i2,dim=2, mask=i2<-10)
  if (b /= 2) call abort
  if (b2 /= 0) call abort
  if (b3 /= 2) call abort
  if (b4 /= 1) call abort
  if (any(b5 /= [1, 2])) call abort
  if (any(b6 /= [0, 0])) call abort
  if (any(b7 /= [2, 1])) call abort
  if (any(b8 /= [1, 2])) call abort
  if (any(b9 /= [0, 0])) call abort
  d = 1
  if (any(b10 /= minloc(i2,dim=d))) call abort
  d = 2
  if (any(b11 /= minloc(i2,dim=2))) call abort
  d = 1
  if (any(b12 /= minloc(i2, dim=d,mask=i2>3))) call abort
  if (any(b13 /= 0)) call abort
end program main
