! { dg-do run }
! { dg-shouldfail "ERROR STOP terminates all images" }
! { dg-output "ERROR STOP no way back" }
!
! ERROR STOP on one image has to terminate all other images.  Without that
! the surviving images block in the SYNC ALL below until the test times out.

program error_stop_all_images
  implicit none
  integer :: i

  sync all
  if (this_image () == num_images ()) error stop "no way back"

  do i = 1, 100
    sync all
  end do
end program error_stop_all_images
