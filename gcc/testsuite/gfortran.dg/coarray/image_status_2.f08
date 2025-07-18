! { dg-do run }

program test_image_status_2
  use iso_fortran_env
  implicit none

  type(team_type) :: t
  integer :: i, st
  integer, allocatable :: rem_images(:)

  form team (1, t)

  if (image_status(1) /= 0) error stop "Image 1 should report OK."
  if (image_status(num_images() + 1) /= STAT_STOPPED_IMAGE) error stop "Image should be stopped."

  if (image_status(1, t) /= 0) error stop "Image 1 in team t should report OK."

  if (num_images() > 1) then
    associate (np => num_images())
      sync all
      if (this_image() == 2) fail image 
      rem_images = (/ 1, ( i, i = 3, np )/)
      ! Can't synchronize well on failed image.  Try with a sleep.
      do i = 0, 10
        if (image_status(2) /= STAT_FAILED_IMAGE) then
          call sleep(1)
        else
          exit
        end if
      end do
      sync images (rem_images, stat=st)
      if (image_status(2) /= STAT_FAILED_IMAGE) error stop "Image 2 has NOT status failed."
      if (image_status(2, t) /= STAT_FAILED_IMAGE) error stop "Image 2 has NOT status failed."
    end associate
  end if

end program test_image_status_2

