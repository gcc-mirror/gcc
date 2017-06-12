! { dg-do compile }

program test_image_status_1
  implicit none

  integer :: isv
  integer(kind=1) :: k1
  integer(kind=2) :: k2
  integer(kind=4) :: k4
  integer(kind=8) :: k8

  isv = image_status(1) ! Ok
  isv = image_status(-1)      ! { dg-error "'image' argument of 'image_status' intrinsic at \\(1\\) must be positive" }
  isv = image_status(0)       ! { dg-error "'image' argument of 'image_status' intrinsic at \\(1\\) must be positive" }
  isv = image_status(.true.)  ! { dg-error "'image' argument of 'image_status' intrinsic at \\(1\\) must be INTEGER" }
  isv = image_status([1,2,3]) ! { dg-error "'image' argument of 'image_status' intrinsic at \\(1\\) must be a scalar" }
  isv = image_status(k1) ! Ok
  isv = image_status(k2) ! Ok
  isv = image_status(k4) ! Ok
  isv = image_status(k8) ! Ok
  isv = image_status(1, team=1) ! { dg-error "'team' argument of 'image_status' intrinsic at \\(1\\) not yet supported" }
  isv = image_status()          ! { dg-error "Missing actual argument 'image' in call to 'image_status' at \\(1\\)" }
  isv = image_status(team=1)    ! { dg-error "Missing actual argument 'image' in call to 'image_status' at \\(1\\)" }

end program test_image_status_1

