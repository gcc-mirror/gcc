pure subroutine sub1
  implicit none
  integer :: i
  !$omp target do  ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
  do i = 1, 5
  end do
  !$omp end target  ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
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

pure subroutine sub4
  implicit none
  integer :: i
  !$omp do  ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
  do i = 1, 5
  end do
  !$omp end do  ! { dg-error "OpenMP directive at .1. is not pure and thus may not appear in a PURE procedure" }
end
