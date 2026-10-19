! { dg-do run }
! PR 127684
!
! A SYNC IMAGES that finds a stopped image in its set still counts for the
! correspondence with the other images of the set (F2023 11.7.4), so it
! releases an image whose set does not contain the stopped image.
!
! Image 2 executes its first SYNC IMAGES only after it sees image 3 stopped;
! image 1 may wait for it already or arrive later.  Image 1 checks the
! pairing of the next one through X.  Without the fix image 1 pairs its first
! SYNC IMAGES with the second one of image 2 and the test hangs.

program sync_images_stopped_3
  use iso_fortran_env, only : stat_stopped_image
  implicit none
  integer :: st, x[*]

  if (num_images () < 3) stop
  x = 0
  sync all

  select case (this_image ())
  case (1)
    st = -1
    sync images (2, stat=st)
    if (st /= 0) stop 1
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
    stop
  end select
end program sync_images_stopped_3
