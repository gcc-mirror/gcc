! { dg-do run }
! { dg-output "^" }
!
! Test fix for pr24785 - EOR used to scrub the 2X.
! Reduced from PR example submitted by Harald Anlauf <anlauf@gmx.de>
!
     program x_with_advance_bug
     write (*,'(A,2X)',  advance="no") "<"
     write (*,'(A)') ">" ! { dg-output "<  >" }
     end
