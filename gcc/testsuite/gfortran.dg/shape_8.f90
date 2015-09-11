! { dg-do run }
!
! Check that we can use SHAPE with optional kind argument
!
program test
   implicit none
   real, allocatable :: x(:,:)

   allocate(x(2,5))
   if (any(shape(x) /= [ 2, 5 ])) call abort
   if (any(shape(x,kind=1) /= [ 2, 5 ])) call abort
   if (any(shape(x,kind=2) /= [ 2, 5 ])) call abort
   if (any(shape(x,kind=4) /= [ 2, 5 ])) call abort
   if (any(shape(x,kind=8) /= [ 2, 5 ])) call abort
end
