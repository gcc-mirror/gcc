! { dg-do run }
!
! PR fortran/38282
!
implicit none
integer :: a(2,1)

a(1,1) = 35
a(2,1) = -74

if (iand(a(1,1),a(2,1)) /= iall(a)) STOP 1
if (iand(a(1,1),a(2,1)) /= iall(array=[35, -74])) STOP 2
if (any (iand(a(1,1),a(2,1)) /= iall(a,dim=1))) STOP 3
if (iand(a(1,1),a(2,1)) /= iall(dim=1,mask=[.true.,.true.],array=[35, -74])) STOP 4

if (ior(a(1,1),a(2,1)) /= iany(a)) STOP 5
if (ior(a(1,1),a(2,1)) /= iany(array=[35, -74])) STOP 6
if (any (ior(a(1,1),a(2,1)) /= iany(a,dim=1))) STOP 7
if (ior(a(1,1),a(2,1)) /= iany(dim=1,mask=[.true.,.true.],array=[35, -74])) STOP 8

if (ieor(a(1,1),a(2,1)) /= iparity(a)) STOP 9
if (ieor(a(1,1),a(2,1)) /= iparity(array=[35, -74])) STOP 10
if (any (ieor(a(1,1),a(2,1)) /= iparity(a,dim=1))) STOP 11
if (ieor(a(1,1),a(2,1)) /= iparity(dim=1,mask=[.true.,.true.],array=[35, -74])) STOP 12

end
