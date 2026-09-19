! { dg-do run }
! PR 127702
!
! NUM_IMAGES and IMAGE_STATUS still count an image that has stopped.

program num_images_stopped_1
  use iso_fortran_env, only: stat_stopped_image
  implicit none
  integer :: n, st

  n = num_images ()
  if (n == 1) stop
  sync all
  if (this_image () == 1) stop

  do
    sync all (stat=st)
    if (st == stat_stopped_image) exit
  end do
  if (num_images () /= n) stop 1
  if (image_status (n) /= 0) stop 2
  if (image_status (1) /= stat_stopped_image) stop 3
end program num_images_stopped_1
