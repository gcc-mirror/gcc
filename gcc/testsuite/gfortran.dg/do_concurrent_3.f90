! { dg-do compile }
! PR 56519 - flag impure intrinsic subroutine calls
! within DO CONCURRENT
program main
  implicit none
  integer :: i
  real :: array(123), val

  do concurrent (i = 1:123)
     call random_number (val) ! { dg-error "is not PURE" }
     array(i) = val
  end do
end program main
