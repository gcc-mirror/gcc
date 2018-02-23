! { dg-do  run }
! Check that simplification of maxloc works
program main
  implicit none
  integer :: d
  real, dimension(2), parameter :: a = [1.0, 0.0]
  character(len=3), dimension(3), parameter :: c = [ "fgh", "asd", "jkl" ]
  integer, parameter :: b = maxloc(a,dim=1)
  integer, parameter :: b2 = maxloc(a,dim=1,mask=[.false.,.false.])
  integer, parameter :: b3 = maxloc(c,dim=1)
  integer, parameter :: b4 = maxloc(c,dim=1,mask=[c<"iii"])
  integer, parameter,dimension(2,2) :: i1 = reshape([4,5,3,2],shape(i1))
  integer, parameter, dimension(2) :: b5 = maxloc(i1)
  integer, parameter, dimension(2) :: b6 = maxloc(i1,mask=i1>7)
  integer, parameter, dimension(2) :: b7 = maxloc(i1, mask=i1<5)
  integer, parameter, dimension(2) :: b8 = maxloc(i1, mask=.true.)
  integer, parameter, dimension(2) :: b9 = maxloc(i1, mask=.false.)
  integer, parameter, dimension(2,3) :: i2 = &
       reshape([2, -1, -3, 4, -5, 6], shape(i2))
  integer, parameter, dimension(3) :: b10 = maxloc(i2, dim=1)
  integer, parameter, dimension(2) :: b11 = maxloc(i2, dim=2)
  integer, parameter, dimension(3) :: b12 = maxloc(i2,dim=1,mask=i2<0)
  integer, parameter, dimension(2) :: b13 = maxloc(i2,dim=2, mask=i2<-10)
  if (b /= 1) STOP 1
  if (b2 /= 0) STOP 2
  if (b3 /= 3) STOP 3
  if (b4 /= 1) STOP 4
  if (any(b5 /= [2,1])) STOP 5
  if (any(b6 /= [0, 0])) STOP 6
  if (any(b7 /= [1,1])) STOP 7
  if (any(b8 /= b5)) STOP 8
  if (any(b9 /= [0, 0])) STOP 9
  d = 1
  if (any(b10 /= maxloc(i2,dim=d))) STOP 10
  d = 2
  if (any(b11 /= maxloc(i2,dim=2))) STOP 11
  d = 1
  if (any(b12 /= maxloc(i2, dim=d,mask=i2<0))) STOP 12
  if (any(b13 /= 0)) STOP 13
end program main
