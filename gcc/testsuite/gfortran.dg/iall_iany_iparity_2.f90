! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/38282
!
implicit none
integer :: a(2,1)

a(1,1) = 35
a(2,1) = -74

if (iand(a(1,1),a(2,1)) /= iall(a)) stop 1 ! { dg-error " .iall. at .1. has no IMPLICIT type" }

if (ior(a(1,1),a(2,1)) /= iany(a)) stop 1 ! { dg-error " .iany. at .1. has no IMPLICIT type" }

if (ieor(a(1,1),a(2,1)) /= iparity(a)) stop 1 ! { dg-error " .iparity. at .1. has no IMPLICIT type" }

end
