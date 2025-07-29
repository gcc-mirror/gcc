! { dg-do compile }

program test_stopped_images_1
  implicit none

  integer, allocatable :: gi(:)
  real :: r
  integer :: i

  gi = stopped_images()         ! OK
  gi = stopped_images(TEAM=1)   ! { dg-error "'team' argument of 'stopped_images' intrinsic at \\(1\\) not yet supported" }
  gi = stopped_images(KIND=1)   ! OK
  gi = stopped_images(KIND=4)   ! OK
  gi = stopped_images(KIND=0)   ! { dg-error "'kind' argument of 'stopped_images' intrinsic at \\\(1\\\) must be positive" }
  gi = stopped_images(KIND=r)   ! { dg-error "'kind' argument of 'stopped_images' intrinsic at \\\(1\\\) must be INTEGER" }
  gi = stopped_images(KIND=i)   ! { dg-error "Constant expression required at \\\(1\\\)" }
  gi = stopped_images(KIND=42)  ! { dg-error "'kind' argument of 'stopped_images' intrinsic at \\\(1\\\) shall specify a valid integer kind" }

end program test_stopped_images_1

