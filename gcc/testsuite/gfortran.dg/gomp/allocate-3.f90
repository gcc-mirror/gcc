! { dg-do compile }

subroutine foo(x)
  implicit none
  integer  :: x
  integer  :: i

  !$omp parallel do simd private (x) allocate (x) ! { dg-error "'x' specified in 'allocate' clause at .1. but not in an explicit privatization clause" }
  do i = 1, 64
    x = i
  end do
  !$omp end parallel do simd

end subroutine
