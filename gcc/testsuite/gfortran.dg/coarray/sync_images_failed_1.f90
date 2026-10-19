! { dg-do run }
! PR 127684
!
! SYNC IMAGES (STAT=) with a failed image in the image set synchronizes the
! active images of the set (F2023 11.7.11), both for an image that already
! waits in the statement when the image fails and for one that arrives later.
!
! Image 1 tells image 3 that it is about to wait for images 2 and 3; image 3
! spins to give it time to start waiting, then fails.  Image 2 executes its
! SYNC IMAGES only after it sees image 3 failed, and image 1 reads X from
! image 2 to check that the two synchronized.  Without the fix the test hangs.

program sync_images_failed_1
  use iso_fortran_env, only : atomic_int_kind, stat_failed_image
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
    if (st /= stat_failed_image) stop 1
    if (x[2] /= 42) stop 2
  case (2)
    do while (image_status (3) /= stat_failed_image)
    end do
    x = 42
    st = 0
    sync images ([1, 3], stat=st)
    if (st /= stat_failed_image) stop 3
  case (3)
    a = 0
    do while (a == 0)
      call atomic_ref (a, arrived)
    end do
    call spin ()
    fail image
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
end program sync_images_failed_1
