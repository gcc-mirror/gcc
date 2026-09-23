! { dg-do run }
!
! SYNC IMAGES (*) has to keep working after an image terminated normally.

program sync_images_stopped_1
  implicit none
  integer :: i, st

  sync all
  if (num_images () > 1 .and. this_image () == num_images ()) stop

  do i = 1, 5
    st = 0
    sync images (*, stat=st)
  end do
end program sync_images_stopped_1
