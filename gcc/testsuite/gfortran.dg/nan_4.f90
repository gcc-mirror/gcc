! { dg-do compile }
! { dg-options "-std=gnu" } 
! { dg-options "-std=gnu -mieee" { target alpha*-*-* sh*-*-* } } 
!
! PR fortran/34398.
!
! Check for invalid numbers in bit-wise BOZ transfers
!
program test
  implicit none
  real(4), parameter :: r0 = z'FFFFFFFF' ! { dg-error "Arithmetic NaN" }
  real(4) r
  data r/z'FFFFFFFF'/ ! { dg-error "Arithmetic NaN" }
  r = z'FFFFFFFF' ! { dg-error "Arithmetic NaN" }
end program test
