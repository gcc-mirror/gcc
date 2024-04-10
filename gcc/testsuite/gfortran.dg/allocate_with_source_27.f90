!
! { dg-do run }
!
! fortran/PR114024
! https://github.com/fujitsu/compiler-test-suite
! Modified from Fortran/0093/0093_0130.f90
!
program foo
   implicit none
   complex :: cmp(3)
   real, allocatable :: xx(:), yy(:), zz(:)
   cmp = (3., 6.78)
   allocate(xx, source = cmp%re)          ! This caused an ICE.
   allocate(yy, source = cmp(1:3)%re)     ! This caused an ICE.
   allocate(zz, source = (cmp%re))
   if (any(xx /= [3., 3., 3.])) stop 1
   if (any(yy /= [3., 3., 3.])) stop 2
   if (any(zz /= [3., 3., 3.])) stop 3
end program foo

