! { dg-do run }
!
! SYNC ALL (STAT=) with a stopped image in the team has the effect of
! SYNC MEMORY: it must not wait for the other images (F2023 11.7.11).

program sync_memory_stopped_1
  use iso_fortran_env, only : stat_stopped_image
  implicit none
  integer :: s

  if (num_images () < 3) stop
  sync all
  if (this_image () == 3) stop
  do while (image_status (3) /= stat_stopped_image)
  end do
  s = -1
  if (this_image () == 1) then
    sync all (stat=s)
    sync images (2)
  else if (this_image () == 2) then
    sync images (1)
    sync all (stat=s)
  end if
  if (this_image () <= 2 .and. s /= stat_stopped_image) error stop 1
end program sync_memory_stopped_1
