! { dg-do run }
!
! FAIL IMAGE on one image must not block the remaining images in SYNC ALL.
! They synchronize among themselves and get STAT_FAILED_IMAGE.

program fail_image_sync_1
  use iso_fortran_env, only: STAT_FAILED_IMAGE
  implicit none
  integer :: i, st

  sync all
  if (num_images () > 1 .and. this_image () == num_images ()) fail image

  do i = 1, 5
    st = 0
    sync all (stat=st)
  end do
  if (num_images () > 1 .and. st /= STAT_FAILED_IMAGE) stop 1
end program fail_image_sync_1
