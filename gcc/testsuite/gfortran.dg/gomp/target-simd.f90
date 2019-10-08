! { dg-do compile }

program test
  implicit none
  real, allocatable :: a(:), b(:)
  integer :: i

  a = [(i, i = 1, 100)]
  allocate(b, mold=a)
  b = 0

  !$omp target simd map(to:a) map(from:b)
  do i = 0, size(a)
    b(i) = 5.0 * a(i)
  end do

  if (any (b - 5.0 *a > 10.0*epsilon(a))) call abort()

  !$omp target simd map(to:a) map(from:b)
  do i = 0, size(a)
    b(i) = 2.0 * a(i)
  end do
  !$omp end target simd

  if (any (b - 2.0 *a > 10.0*epsilon(a))) call abort()
end program test
