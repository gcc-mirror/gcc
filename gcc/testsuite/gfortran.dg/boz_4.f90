! { dg-do compile }
! { dg-options "-fallow-invalid-boz" }
!
program boz
   implicit none
   integer(1), parameter :: b1 = b'0101010110101010'                    ! { dg-warning "BOZ literal constant" }
   integer(2), parameter :: b2 = b'01110000111100001111000011110000'    ! { dg-warning "BOZ literal constant" }
   integer(4), parameter :: &
   &  b4 = b'0111000011110000111100001111000011110000111100001111000011110000' ! { dg-warning "BOZ literal constant" }
   integer(1), parameter :: o1 = o'1234567076543210' ! { dg-warning "BOZ literal constant" }
   integer(2), parameter :: o2 = o'1234567076543210' ! { dg-warning "BOZ literal constant" }
   integer(4), parameter :: o4 = o'1234567076543210' ! { dg-warning "BOZ literal constant" }
   integer(1), parameter :: z1 = z'deadbeef'         ! { dg-warning "BOZ literal constant" }
   integer(2), parameter :: z2 = z'deadbeef'         ! { dg-warning "BOZ literal constant" }
   integer(4), parameter :: z4 = z'deadbeeffeed'     ! { dg-warning "BOZ literal constant" }
end program boz
