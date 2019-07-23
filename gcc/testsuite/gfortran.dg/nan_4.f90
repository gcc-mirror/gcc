! { dg-do compile }
! { dg-options "-std=gnu -fallow-invalid-boz" } 
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
! PR fortran/34398.
!
! Check for invalid numbers in bit-wise BOZ transfers
!
program test
  implicit none
  real(4), parameter :: r0 = z'FFFFFFFF'
  real(4) r
  data r/z'FFFFFFFF'/
  r = z'FFFFFFFF'       ! { dg-warning "neither a DATA statement value" }
end program test
