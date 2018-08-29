! PR c/81887
! { dg-do compile }
! { dg-options "-fno-openmp -fopenmp-simd -fdump-tree-gimple" }
! { dg-final { scan-tree-dump-times "#pragma omp simd" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp ordered simd\[ \t]*\[\n\r]" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp" 4 "gimple" } }

subroutine f1 (x)
  integer :: i, x(100)
  !$omp simd
  do i = 2, 101
    !$omp ordered simd
    x(i / 2) = i
    !$omp end ordered
  end do
end subroutine

subroutine f2 (x)
  integer :: i, x(100)
  !$omp parallel do simd ordered
  do i = 2, 101
    !$omp ordered threads simd
    x(i / 2) = i
    !$omp end ordered
  end do
end subroutine

subroutine f3 (x)
  integer :: i, x(100)
  !$omp parallel do ordered
  do i = 2, 101
    !$omp ordered
    x(i / 2) = i
    !$omp end ordered
  end do
end subroutine

subroutine f4 (x)
  integer :: i, x(100)
  !$omp parallel do ordered
  do i = 2, 101
    !$omp ordered threads
    x(i / 2) = i
    !$omp end ordered
  end do
end subroutine

subroutine f5(x, n)
  integer :: i, j, k, n, x(100,100,100)
  !$omp parallel do ordered(3)
  do i = 1, n
    do j = 1, n
      do k = 1, n
	!$omp ordered depend(sink:i-8,j-2,k+2) depend(sink:i, j-1,k) depend(sink:i-4,j-3,k+6) depend(sink:i-6,j-4,k-6)
	x(i, j, k) = i + j + k
	!$omp ordered depend(source)
      end do
    end do
  end do
  !$omp end parallel do
end subroutine
