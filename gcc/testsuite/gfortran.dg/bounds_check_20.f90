! { dg-do  run }
! { dg-additional-options "-fcheck=bounds -ffrontend-optimize" }
! PR 85631 - this used to cause a runtime error with bounds checking.
module x
contains
  subroutine sub(a, b)
    real, dimension(:,:), intent(in) :: a
    real, dimension(:,:), intent(out), allocatable :: b
    b = transpose(a)
  end subroutine sub
end module x

program main
  use x
  implicit none
  real, dimension(2,2) :: a
  real, dimension(:,:), allocatable :: b
  data a /-2., 3., -5., 7./
  call sub(a, b)
  if (any (b /= reshape([-2., -5., 3., 7.], shape(b)))) stop 1
  b = matmul(transpose(b), a)
  if (any (b /= reshape([-11., 15., -25.,  34.], shape(b)))) stop 2
end program

