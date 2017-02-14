! { dg-do compile }
! { dg-options "" }
!
! Test compile-time errors for DEC I/O intrinsics without -fdec.
!

integer :: fd
open (unit=fd, carriagecontrol='cc') ! { dg-error "is a DEC extension" }
open (unit=fd, share='cc')           ! { dg-error "is a DEC extension" }
open (unit=fd, shared)               ! { dg-error "is a DEC extension" }
open (unit=fd, noshared)             ! { dg-error "is a DEC extension" }
open (unit=fd, readonly)             ! { dg-error "is a DEC extension" }
close (unit=fd, status='delete')

end
