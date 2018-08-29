! { dg-do run }
! { dg-options "-std=gnu" }
! Test the boz handling
program boz

   implicit none

   integer(1), parameter :: b1 = b'00000001'
   integer(2), parameter :: b2 = b'0101010110101010'
   integer(4), parameter :: b4 = b'01110000111100001111000011110000'
   integer(8), parameter :: &
   &  b8 = b'0111000011110000111100001111000011110000111100001111000011110000'

   integer(1), parameter :: o1 = o'12'
   integer(2), parameter :: o2 = o'4321'
   integer(4), parameter :: o4 = o'43210765'
   integer(8), parameter :: o8 = o'1234567076543210'

   integer(1), parameter :: z1 = z'a'
   integer(2), parameter :: z2 = z'ab'
   integer(4), parameter :: z4 = z'dead'
   integer(8), parameter :: z8 = z'deadbeef'

   if (z1 /= 10_1) STOP 1
   if (z2 /= 171_2) STOP 2
   if (z4 /= 57005_4) STOP 3
   if (z8 /= 3735928559_8) STOP 4

   if (b1 /= 1_1) STOP 5
   if (b2 /= 21930_2) STOP 6
   if (b4 /= 1894838512_4) STOP 7
   if (b8 /= 8138269444283625712_8) STOP 8

   if (o1 /= 10_1) STOP 9
   if (o2 /= 2257_2) STOP 10
   if (o4 /= 9245173_4) STOP 11
   if (o8 /= 45954958542472_8) STOP 12

end program boz
