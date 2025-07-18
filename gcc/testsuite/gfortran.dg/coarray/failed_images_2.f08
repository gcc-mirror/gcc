! { dg-do run }

program test_failed_images_2
  use iso_fortran_env
  implicit none

  type(team_type) :: t
  integer, allocatable :: fi(:)
  integer(kind=1), allocatable :: sfi(:)
  integer, allocatable :: rem_images(:)
  integer :: i, st

  associate(np => num_images())
    form team (1, t)
    fi = failed_images()
    if (size(fi) > 0) stop 1
    sfi = failed_images(KIND=1)
    if (size(sfi) > 0) stop 2
    sfi = failed_images(KIND=8)
    if (size(sfi) > 0) stop 3
    
    fi = failed_images(t)
    if (size(fi) > 0) stop 4
  
    if (num_images() > 1) then
      sync all
      if (this_image() == 2) fail image
      rem_images = (/ 1, ( i, i = 3, np )/)
      ! Can't synchronize well on a failed image.  Try with a sleep.
      do i = 0, 10
        if (size(failed_images()) == 0) then
          call sleep(1)
        else
          exit
        end if
      end do
      if (i == 10 .AND. size(failed_images()) == 0) stop 5
      sync images (rem_images, stat=st)
      if (any(failed_images() /= [2])) stop 6
      if (any(failed_images(t, 8) /= [2])) stop 7
    end if
  end associate
end program test_failed_images_2

