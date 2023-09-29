! { dg-do run }
! PR fortran/49588 - vector sections in data statements

block data
  implicit none
  integer :: a(8), b(3,2), i
  data a(::2)   /4*1/
  data a([2,6]) /2*2/
  data a([4])   /3/
  data a([(6+2*i,i=1,1)]) /1*5/
  data b( 1   ,[1,2]) /11,12/
  data b([2,3],[2,1]) /22,32,21,31/
  common /com/ a, b
end block data

program test
  implicit none
  integer :: a(8), b(3,2), i, j
  common /com/ a, b
  print *, a
  print *, b
! print *, a - [1,2,1,3,1,2,1,5]
! print *, ((b(i,j)-(10*i+j),i=1,3),j=1,2)
  if (.not. all (a == [1,2,1,3,1,2,1,5])) stop 1
  if (.not. all (b == reshape ([((10*i+j,i=1,3),j=1,2)], shape (b)))) stop 2
end program test
