! { dg-do  run }
program main
  implicit none
  integer :: i
  integer, parameter, dimension(3,3) :: a = &
       reshape([1,2,3,4,5,6,7,8,9], shape(a))
  integer, dimension(3,3) :: b
  integer, parameter, dimension(3,4,5) :: c = &
       reshape([(i**2,i=1,3*4*5)],shape(c))
  integer, dimension(3,4,5) :: d
  integer, dimension(4,5), parameter :: sh1 =&
       reshape([(i**3-12*i**2,i=1,4*5)],shape(sh1))
  integer, dimension(3,5), parameter :: sh2 = &
       reshape([(i**3-7*i**2,i=1,3*5)], shape(sh2))
  integer, dimension(3,4), parameter :: sh3 = &
       reshape([(i**3-3*i**2,i=1,3*4)], shape(sh3))
  integer, parameter, dimension(3,4,5) :: c1 = cshift(c,shift=sh1,dim=1)
  integer, parameter, dimension(3,4,5) :: c2 = cshift(c,shift=sh2,dim=2)
  integer, parameter, dimension(3,4,5) :: c3 = cshift(c,shift=sh3,dim=3)

  b = a
  if (any(cshift(a,1) /= cshift(b,1))) STOP 1
  if (any(cshift(a,2) /= cshift(b,2))) STOP 2
  if (any(cshift(a,1,dim=2) /= cshift(b,1,dim=2))) STOP 3
  d = c
  if (any(cshift(c,1) /= cshift(d,1))) STOP 4
  if (any(cshift(c,2) /= cshift(d,2))) STOP 5
  if (any(cshift(c,3) /= cshift(d,3))) STOP 6

  if (any(cshift(c,1,dim=2) /= cshift(d,1,dim=2))) STOP 7
  if (any(cshift(c,2,dim=2) /= cshift(d,2,dim=2))) STOP 8
  if (any(cshift(c,3,dim=3) /= cshift(d,3,dim=3))) STOP 9

  if (any(cshift(d,shift=sh1,dim=1) /= c1)) STOP 10
  if (any(cshift(d,shift=sh2,dim=2) /= c2)) STOP 11
  if (any(cshift(d,shift=sh3,dim=3) /= c3)) STOP 12
end program main
