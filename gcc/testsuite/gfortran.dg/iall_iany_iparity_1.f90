! { dg-do run }
!
! PR fortran/38282
!
implicit none
integer :: a(2,1)

a(1,1) = 35
a(2,1) = -74

if (iand(a(1,1),a(2,1)) /= iall(a)) call abort ()
if (iand(a(1,1),a(2,1)) /= iall(array=[35, -74])) call abort ()
if (any (iand(a(1,1),a(2,1)) /= iall(a,dim=1))) call abort ()
if (iand(a(1,1),a(2,1)) /= iall(dim=1,mask=[.true.,.true.],array=[35, -74])) call abort ()

if (ior(a(1,1),a(2,1)) /= iany(a)) call abort ()
if (ior(a(1,1),a(2,1)) /= iany(array=[35, -74])) call abort ()
if (any (ior(a(1,1),a(2,1)) /= iany(a,dim=1))) call abort ()
if (ior(a(1,1),a(2,1)) /= iany(dim=1,mask=[.true.,.true.],array=[35, -74])) call abort ()

if (ieor(a(1,1),a(2,1)) /= iparity(a)) call abort ()
if (ieor(a(1,1),a(2,1)) /= iparity(array=[35, -74])) call abort ()
if (any (ieor(a(1,1),a(2,1)) /= iparity(a,dim=1))) call abort ()
if (ieor(a(1,1),a(2,1)) /= iparity(dim=1,mask=[.true.,.true.],array=[35, -74])) call abort ()

end
