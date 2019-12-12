! { dg-do compile }
! { dg-options "-std=gnu -fallow-invalid-boz" } 
! { dg-add-options ieee }
!
! PR fortran/34398.
!
! Check for invalid numbers in bit-wise BOZ transfers
!
program test
  implicit none
  real(4), parameter :: r0 = z'FFFFFFFF' ! { dg-warning "BOZ literal constant" }
  real(4) r
  data r/z'FFFFFFFF'/   ! { dg-warning "BOZ literal constant" }
  r = z'FFFFFFFF'       ! { dg-warning "BOZ literal constant" }
end program test
