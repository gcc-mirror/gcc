! { dg-do run }
! PR libfortran/127751
!
! ALLOCATE of a coarray whose total size over all images does not fit in
! size_t reports a nonzero STAT= instead of allocating a wrapped-around size.

program alloc_pool_exhausted_2
  use iso_c_binding, only: c_intptr_t
  implicit none
  ! 2**62 bytes per image on 64-bit targets, 2**30 on 32-bit targets
  integer(8), parameter :: n = 2_8**(bit_size (0_c_intptr_t) - 5)
  integer(8), allocatable :: a(:)[:]
  integer :: st, nb

  ! With four images the total size wraps to zero
  if (num_images () < 4) stop
  allocate (a(n)[*], stat=st)
  if (st == 0) stop 1
  allocate (a(10)[*], stat=st)
  if (st /= 0) stop 2
  a = this_image ()
  sync all
  nb = mod (this_image (), num_images ()) + 1
  if (a(10)[nb] /= nb) stop 3
end program alloc_pool_exhausted_2
