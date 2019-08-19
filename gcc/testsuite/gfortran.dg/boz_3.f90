! { dg-do run }
! { dg-options "-std=gnu -fallow-invalid-boz" }
!
! Test that the BOZ constant on the RHS, which are of different KIND than
! the LHS, are correctly converted.
!
program boz

   implicit none

   integer(1), parameter :: &
   &  b1 = b'000000000001111'                   ! { dg-warning "BOZ literal constant at" }
   integer(2), parameter :: &
   &  b2 = b'00000000000000000111000011110000'  ! { dg-warning "BOZ literal constant at" }
   integer(4), parameter :: &
   &  b4 = b'0000000000000000000000000000000001110000111100001111000011110000'  ! { dg-warning "BOZ literal constant at" }

   integer(1), parameter :: o1 = o'0012'              ! { dg-warning "BOZ literal constant at" }
   integer(2), parameter :: o2 = o'0004321'           ! { dg-warning "BOZ literal constant at" }
   integer(4), parameter :: o4 = o'0000000043210765'  ! { dg-warning "BOZ literal constant at" }

   integer(1), parameter :: z1 = z'0a'       ! { dg-warning "BOZ literal constant at" }
   integer(2), parameter :: z2 = z'00ab'     ! { dg-warning "BOZ literal constant at" }
   integer(4), parameter :: z4 = z'0000dead' ! { dg-warning "BOZ literal constant at" }

   if (b1 /= 15_1) STOP 1
   if (b2 /= 28912_2) STOP 2
   if (b4 /= 1894838512_4) STOP 3

   if (o1 /= 10_1) STOP 4
   if (o2 /= 2257_2) STOP 5
   if (o4 /= 9245173_4) STOP 6

   if (z1 /= 10_1) STOP 7
   if (z2 /= 171_2) STOP 8
   if (z4 /= 57005_4) STOP 9

end program boz
