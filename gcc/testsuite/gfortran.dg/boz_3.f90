! { dg-do run }
! Test that the BOZ constant on the RHS, which are of different KIND than
! the LHS, are correctly converted.
!
program boz

   implicit none

   integer(1), parameter :: b1 = b'000000000001111'
   integer(2), parameter :: b2 = b'00000000000000000111000011110000'
   integer(4), parameter :: &
   &  b4 = b'0000000000000000000000000000000001110000111100001111000011110000'

   integer(1), parameter :: o1 = o'0012'
   integer(2), parameter :: o2 = o'0004321'
   integer(4), parameter :: o4 = o'0000000043210765'

   integer(1), parameter :: z1 = z'0a'
   integer(2), parameter :: z2 = z'00ab'
   integer(4), parameter :: z4 = z'0000dead'

   if (b1 /= 15_1) call abort
   if (b2 /= 28912_2) call abort
   if (b4 /= 1894838512_4) call abort

   if (o1 /= 10_1) call abort
   if (o2 /= 2257_2) call abort
   if (o4 /= 9245173_4) call abort

   if (z1 /= 10_1) call abort
   if (z2 /= 171_2) call abort
   if (z4 /= 57005_4) call abort

end program boz
