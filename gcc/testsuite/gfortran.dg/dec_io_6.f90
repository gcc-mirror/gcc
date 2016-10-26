! { dg-do run "xfail *-*-*" }
! { dg-options "-fdec" }
!
! Test that we get a run-time error for close-on-delete with READONLY.
!

implicit none

integer :: fd = 8
character(*), parameter :: f = "test.txt"

open(unit=fd,file=f,action='read',readonly)
close(unit=fd,status='delete') ! XFAIL "protected by READONLY"

end
