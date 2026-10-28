! { dg-do run }
! PR 127751
!
! ALLOCATE of a coarray that does not fit into the shared memory reports a
! nonzero STAT= instead of handing out memory past its end.

program alloc_pool_exhausted_1
  use iso_c_binding, only: c_intptr_t
  implicit none
  ! Per-image element count; kept in range of a 32-bit index and size_t
  integer(8), parameter :: n = merge (2_8**37, 2_8**25, &
                                      bit_size (0_c_intptr_t) == 64)
  integer(8), allocatable :: a(:)[:]
  integer :: st, nb

  if (num_images () < 2) stop
  ! 1 TiB per image on 64-bit targets, 256 MiB on 32-bit targets
  allocate (a(n)[*], stat=st)
  if (st == 0) stop 1
  allocate (a(10)[*], stat=st)
  if (st /= 0) stop 2
  a = this_image ()
  sync all
  nb = mod (this_image (), num_images ()) + 1
  if (a(10)[nb] /= nb) stop 3
end program alloc_pool_exhausted_1
