! { dg-do compile }
! 
program test
  implicit none
  real x4
  double precision x8

  x4 = 1.7
  x8 = 1.7
  write(*,*) complex(x4,z'1FFFFFFFF') ! { dg-error "too" }
  write(*,*) cmplx(x8,z'1FFFFFFFFFFFFFFFF') ! { dg-error "too" }
  write(*,*) complex(x8,z'1FFFFFFFFFFFFFFFF') ! { dg-error "too" }
  write(*,*) dcmplx(x8,z'1FFFFFFFFFFFFFFFF') ! { dg-error "too" }
end program test
