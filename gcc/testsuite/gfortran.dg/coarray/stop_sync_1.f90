! { dg-do run }
!
! A normal STOP on one image must not block the surviving images in the
! SYNC ALL statements that follow.

program stop_sync_1
  implicit none
  integer :: i, st

  sync all
  if (num_images () > 1 .and. this_image () == num_images ()) stop

  do i = 1, 5
    st = 0
    sync all (stat=st)
  end do
end program stop_sync_1
