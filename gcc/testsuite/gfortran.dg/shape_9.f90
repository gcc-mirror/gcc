! { dg-do  run }
! { dg-require-effective-target lto }
! { dg-options "-flto" }
! Check that there are no warnings with LTO for a KIND argument.
!
program test
   implicit none
   real, allocatable :: x(:,:)

   allocate(x(2,5))
   if (any(shape(x) /= [ 2, 5 ])) STOP 1
   if (any(shape(x,kind=1) /= [ 2, 5 ])) STOP 2
   if (any(shape(x,kind=2) /= [ 2, 5 ])) STOP 3
   if (any(shape(x,kind=4) /= [ 2, 5 ])) STOP 4
   if (any(shape(x,kind=8) /= [ 2, 5 ])) STOP 5
 end program test
