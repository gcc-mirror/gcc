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

subroutine bar(a)
  implicit none
  integer  :: a
!$omp target
  !$omp parallel private (a) allocate(a) ! { dg-error "'allocate' clause must specify an allocator here" }
    a = 20
  !$omp end parallel
!$omp end target

!$omp target private(a) allocate(a) ! { dg-error "'allocate' clause must specify an allocator here" }
  a = 30;
!$omp end target
end subroutine
