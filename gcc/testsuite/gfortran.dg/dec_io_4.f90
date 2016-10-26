! { dg-do compile }
! { dg-options "-fdec" }
!
! Test compile-time errors for DEC I/O intrinsics with -fdec.
!

integer :: fd
open (unit=fd, readonly, action='read') ! these are okay
open (unit=fd, action='read', readonly)
open (unit=fd, readonly, action='write') ! { dg-error "ACTION type conflicts" }
open (unit=fd, action='readwrite', readonly) ! { dg-error "ACTION type conflicts" }
open (unit=fd, shared, shared)             ! { dg-error "Duplicate SHARE" }
open (unit=fd, noshared, shared)             ! { dg-error "Duplicate SHARE" }
open (unit=fd, share='denyrw', share='denynone') ! { dg-error "Duplicate SHARE" }
open (unit=fd, carriagecontrol='fortran', carriagecontrol='none') ! { dg-error "Duplicate CARRIAGECONTROL" }

end
