! { dg-options "-fno-openmp -fopenmp-simd -fdump-tree-original" }

subroutine foo (a, b)
  integer, contiguous :: a(:), b(:)
  integer :: i
  !$omp simd reduction (inscan, +:r)
  do i = 1, 1024
    r = r + a(i)
    !$omp scan inclusive(r)
    b(i) = r
  end do
  !$omp end simd

  !$omp loop
  do i = 1, 1024
    a(i) = a(i) + i
  end do
  !$omp end loop
end

! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) reduction\\(inscan,\\+:r\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp scan inclusive\\(r\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp loop" "original" } }
