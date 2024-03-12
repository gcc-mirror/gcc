! { dg-options "-fno-openmp -fopenmp-simd" }

! Invalid combined directives with SIMD in PURE

pure subroutine sub1
  implicit none
  integer :: i
  !$omp target do  ! OK - not parsed by -fopenmp-simd
  do i = 1, 5
  end do
  !$omp end target
end

subroutine sub2
  implicit none
  integer :: i
  !$omp target simd  ! OK - not pure
  do i = 1, 5
  end do
  !$omp end target simd
end

pure subroutine sub3
  implicit none
  integer :: i
  !$omp target simd  ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
  do i = 1, 5
  end do
  !$omp end target simd  ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
end

