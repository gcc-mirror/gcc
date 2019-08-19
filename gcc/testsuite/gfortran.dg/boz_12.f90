! { dg-do compile }
! 
program test
  implicit none
  real x4
  double precision x8
  x4 = 1.7
  x8 = 1.7
  write(*,*) cmplx(x8,z'1FFFFFFFFFFFFFFFF')
  write(*,*) dcmplx(x8,z'1FFFFFFFFFFFFFFFF')
end program test
