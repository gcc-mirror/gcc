! { dg-do run }

program test_stopped_images_2
  implicit none

  integer, allocatable :: si(:)
  integer(kind=1), allocatable :: ssi(:)

  si = stopped_images()
  if (size(si) > 0) error stop "stopped_images result shall be empty array"
  ssi = stopped_images(KIND=1)
  if (size(ssi) > 0) error stop "stopped_images result shall be empty array"
  ssi = stopped_images(KIND=8)
  if (size(ssi) > 0) error stop "stopped_images result shall be empty array"
  
end program test_stopped_images_2

