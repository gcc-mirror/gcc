! { dg-do run }
!
! A normal STOP on one image must not block the surviving images in the
! SYNC ALL statements that follow, which report STAT_STOPPED_IMAGE.

program stop_sync_1
  use iso_fortran_env, only : stat_stopped_image
  implicit none
  integer :: i, st

  sync all
  if (num_images () > 1 .and. this_image () == num_images ()) stop

  do i = 1, 5
    st = 0
    sync all (stat=st)
  end do
  if (num_images () > 1 .and. st /= stat_stopped_image) error stop 1
end program stop_sync_1
