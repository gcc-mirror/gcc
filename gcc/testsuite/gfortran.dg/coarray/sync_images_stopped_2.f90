! { dg-do run }
! PR 127684
!
! SYNC IMAGES (STAT=) with a stopped image in the image set has the effect of
! SYNC MEMORY (F2023 11.7.11), both for an image that already waits in the
! statement when the image stops and for one that arrives later.  The next
! SYNC IMAGES of these two images has to pair with each other.
!
! Image 1 tells image 3 that it is about to wait for images 2 and 3; image 3
! spins to give it time to start waiting, then stops.  Image 2 executes its
! first SYNC IMAGES only after it sees image 3 stopped, and image 1 checks
! the pairing of the next one through X.  Without the fix the test hangs.

program sync_images_stopped_2
  use iso_fortran_env, only : atomic_int_kind, stat_stopped_image
  implicit none
  integer(atomic_int_kind) :: arrived[*], a
  integer :: st, x[*]

  if (num_images () < 3) stop
  arrived = 0
  x = 0
  sync all

  select case (this_image ())
  case (1)
    call atomic_define (arrived[3], 1)
    st = 0
    sync images ([2, 3], stat=st)
    if (st /= stat_stopped_image) stop 1
    sync images (2)
    if (x[2] /= 42) stop 2
  case (2)
    do while (image_status (3) /= stat_stopped_image)
    end do
    st = 0
    sync images ([1, 3], stat=st)
    if (st /= stat_stopped_image) stop 3
    x = 42
    sync images (1)
  case (3)
    a = 0
    do while (a == 0)
      call atomic_ref (a, arrived)
    end do
    call spin ()
    stop
  end select
contains
  subroutine spin ()
    integer :: c
    integer(kind=8) :: v
    v = 2
    do c = 1, 20000000
      v = mod (v * 2, 199679_8)
    end do
  end subroutine
end program sync_images_stopped_2
