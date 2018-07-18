! { dg-do compile }

program test_failed_images_1
  implicit none

  integer, allocatable :: fi(:)
  real :: r
  integer :: i

  fi = failed_images()         ! OK
  fi = failed_images(TEAM=1)   ! { dg-error "'team' argument of 'failed_images' intrinsic at \\(1\\) not yet supported" }
  fi = failed_images(KIND=1)   ! OK
  fi = failed_images(KIND=4)   ! OK
  fi = failed_images(KIND=0)   ! { dg-error "'kind' argument of 'failed_images' intrinsic at \\\(1\\\) must be positive" }
  fi = failed_images(KIND=r)   ! { dg-error "'kind' argument of 'failed_images' intrinsic at \\\(1\\\) must be INTEGER" }
  fi = failed_images(KIND=i)   ! { dg-error "Constant expression required at \\\(1\\\)" }
  fi = failed_images(KIND=42)  ! { dg-error "'kind' argument of 'failed_images' intrinsic at \\\(1\\\) shall specify a valid integer kind" }

end program test_failed_images_1

