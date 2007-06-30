! { dg-do compile }
! { dg-options "-c -Wall" }

subroutine valid
  intrinsic :: abs                 ! ok, intrinsic function
  intrinsic :: cpu_time            ! ok, intrinsic subroutine
end subroutine

subroutine warnings
  ! the follow three are ok in general, but ANY 
  ! type is ignored, even the correct one
  real, intrinsic :: sin           ! { dg-warning "is ignored" }

  real :: asin                     ! { dg-warning "is ignored" }
  intrinsic :: asin

  intrinsic :: tan                 ! { dg-warning "is ignored" }
  real :: tan

  ! wrong types here
  integer, intrinsic :: cos        ! { dg-warning "is ignored" }

  integer :: acos                  ! { dg-warning "is ignored" }
  intrinsic :: acos

  ! ordering shall not matter
  intrinsic :: atan                ! { dg-warning "is ignored" }
  integer :: atan
end subroutine

subroutine errors
  intrinsic :: foo                 ! { dg-error "does not exist" }
  real, intrinsic :: bar           ! { dg-error "does not exist" }

  real, intrinsic :: mvbits        ! { dg-error "shall not have a type" }
end subroutine
