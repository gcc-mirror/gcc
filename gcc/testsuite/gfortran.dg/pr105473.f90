! { dg-do run }
! PR libgfortran/105473
  implicit none
  integer n,m,ios
  real r
  complex z
  character(40):: testinput
  n = 999; m = 777; r=1.2345
  z = cmplx(0.0,0.0)

! Check that semi-colon is not allowed as separator with decimal=point.
  ios=0
  testinput = '1;17;3.14159'
  read(testinput,*,decimal='point',iostat=ios) n, m, r
  if (ios /= 5010) print *, "stop 1"

! Check that comma is not allowed as a separator with decimal=comma.
  ios=0
  testinput = '1,17,3,14159'
  read(testinput,*,decimal='comma',iostat=ios) n, m, r
  if (ios /= 5010) print *, "stop 2"

! Check a good read.
  ios=99
  testinput = '1;17;3,14159'
  read(testinput,*,decimal='comma',iostat=ios) n, m, r
  if (ios /= 0) print *, "stop 3"

! Check that comma is not allowed as a separator with decimal=comma.
  ios=99; z = cmplx(0.0,0.0)
  testinput = '1,17, (3,14159, 1,7182)'
  read(testinput,*,decimal='comma', iostat=ios) n, m, z
  if (ios /= 5010) stop 4

! Check that semi-colon is not allowed as separator with decimal=point.
  ios=99; z = cmplx(0.0,0.0)
  testinput = '1,17; (3.14159; 1.7182)'
  read(testinput,*,decimal='point', iostat=ios) n, m, z
  if (ios /= 5010) stop 5

! Check a good read.
  ios=99;z = cmplx(0.0,0.0)
  testinput = '1;17; (3,14159; 1,7182)'
  read(testinput,*,decimal='comma', iostat=ios) n, m, z
  if (ios /= 0) stop 6
end program
