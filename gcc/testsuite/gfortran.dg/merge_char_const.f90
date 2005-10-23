! { dg-do run }
! { dg-options "-O0" }
! This tests the patch for PR24311 in which the PRINT statement would
! ICE on trying to print a MERGE statement with character constants
! for the first two arguments.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
  integer, dimension(6) :: i = (/1,0,0,1,1,0/)
  print '(6a1)', Merge ("a", "b", i  == 1) ! { dg-output "abbaab" }
  end


