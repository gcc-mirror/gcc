! { dg-do run }
! { dg-options "-fdec" }
! { dg-shouldfail "ACTION conflicts with READONLY" }
!
! Test that we get a run-time error for opening a READONLY file with
! ACTION='WRITE'.
!

implicit none

integer :: fd = 8
character(*), parameter :: f = "dec_io_5.txt"
character(10), volatile :: c
c = 'write'

open(unit=fd,file=f,action=c,readonly)

end
! { dg-output "ACTION conflicts with READONLY" }
