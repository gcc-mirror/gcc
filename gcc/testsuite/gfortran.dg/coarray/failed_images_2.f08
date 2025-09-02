! { dg-do run }

program test_failed_images_2
  implicit none

  integer, allocatable :: fi(:)
  integer(kind=1), allocatable :: sfi(:)

  fi = failed_images()
  if (size(fi) > 0) error stop "failed_images result shall be empty array"
  sfi = failed_images(KIND=1)
  if (size(sfi) > 0) error stop "failed_images result shall be empty array"
  sfi = failed_images(KIND=8)
  if (size(sfi) > 0) error stop "failed_images result shall be empty array"
  
end program test_failed_images_2

