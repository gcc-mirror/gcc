! { dg-do run }

program test_stopped_images_2
  use iso_fortran_env
  implicit none

  type(team_type) :: t
  integer, allocatable :: si(:)
  integer(kind=1), allocatable :: ssi(:)
  integer, allocatable :: rem_images(:)
  integer :: i, st

  associate(np => num_images())
    form team (1, t)
    si = stopped_images()
    if (size(si) > 0) stop 1
    ssi = stopped_images(KIND=1)
    if (size(ssi) > 0) stop 2
    ssi = stopped_images(KIND=8)
    if (size(ssi) > 0) stop 3
    
    si = stopped_images(t)  
    if (size(si) > 0) stop 4
  
    if (num_images() > 1) then
      sync all
      if (this_image() == 2) stop
      rem_images = (/ 1, ( i, i = 3, np )/)
      ! Can't synchronize well on a stopped image.  Try with a sleep.
      do i = 0, 10
        if (size(stopped_images()) == 0) then
          call sleep(1)
        else
          exit
        end if
      end do
      if (i == 10 .AND. size(stopped_images()) == 0) stop 5
      sync images (rem_images, stat=st)
      if (any(stopped_images() /= [2])) stop 6
      if (any(stopped_images(t, 8) /= [2])) stop 7
    end if
  end associate
end program test_stopped_images_2

