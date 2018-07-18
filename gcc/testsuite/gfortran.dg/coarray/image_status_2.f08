! { dg-do run }

program test_image_status_2
  use iso_fortran_env , only : STAT_STOPPED_IMAGE
  implicit none

  if (image_status(1) /= 0) error stop "Image 1 should report OK."
  if (image_status(2) /= STAT_STOPPED_IMAGE) error stop "Image 2 should be stopped."
  if (image_status(3) /= STAT_STOPPED_IMAGE) error stop "Image 3 should be stopped."

end program test_image_status_2

