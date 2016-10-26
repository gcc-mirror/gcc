! { dg-do run "xfail *-*-*" }
! { dg-options "-fdec" }
!
! Test that we get a run-time error for opening a READONLY file with
! ACTION='WRITE'.
!

implicit none

integer :: fd = 8
character(*), parameter :: f = "test.txt"
character(10), volatile :: c
c = 'write'

open(unit=fd,file=f,action=c,readonly) ! XFAIL "ACTION conflicts with READONLY"

end
