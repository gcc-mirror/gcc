! { dg-do compile }
! Test that the conversion of a BOZ constant that is too large for the
! integer variable is caught by the compiler.
program boz

   implicit none

   integer(1), parameter :: &
   &  b1 = b'0101010110101010'  ! { dg-error "overflow converting" }
   integer(2), parameter :: &
   &  b2 = b'01110000111100001111000011110000'  ! { dg-error "overflow converting" }
   integer(4), parameter :: &
   &  b4 = b'0111000011110000111100001111000011110000111100001111000011110000'  ! { dg-error "overflow converting" }

   integer(1), parameter :: &
   &  o1 = o'1234567076543210'  ! { dg-error "overflow converting" }
   integer(2), parameter :: &
   &  o2 = o'1234567076543210'  ! { dg-error "overflow converting" }
   integer(4), parameter :: &
   &  o4 = o'1234567076543210'  ! { dg-error "overflow converting" }

   integer(1), parameter :: &
   &  z1 = z'deadbeef'  ! { dg-error "overflow converting" }
   integer(2), parameter :: &
   &  z2 = z'deadbeef'  ! { dg-error "overflow converting" }
   integer(4), parameter :: &
   &  z4 = z'deadbeeffeed'  ! { dg-error "overflow converting" }

end program boz
