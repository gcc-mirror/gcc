! { dg-additional-options "-fdump-tree-original" }
subroutine foo (p)
  logical :: p(:)
  integer i
  integer a, b, c, d, e, f, g, h
  a = -1; b = -1; c = -1; d = -1; e = -1; f = -1; g = -1; h = -1
  !$omp parallel
  !$omp do lastprivate (conditional: a)
  do i = 1, 32
    if (p(i)) &
      a = i
  end do
  !$omp end parallel
  !$omp simd lastprivate (conditional: b)
  do i = 1, 32
    if (p(i)) &
      b = i
  end do
  !$omp parallel
  !$omp do simd lastprivate (conditional: c)
  do i = 1, 32
    if (p(i)) &
      c = i
  end do
  !$omp end parallel
  !$omp parallel do lastprivate (conditional: d)
  do i = 1, 32
    if (p(i)) &
      d = i
  end do
  !$omp end parallel do
  !$omp parallel do simd lastprivate (conditional: e)
  do i = 1, 32
    if (p(i)) &
      e = i
  end do
  !$omp end parallel do simd
end subroutine

! { dg-final { scan-tree-dump-times "#pragma omp for lastprivate\\(conditional:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:b\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for lastprivate\\(conditional:c\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:c\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel lastprivate\\(conditional:d\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel lastprivate\\(conditional:e\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:e\\)" 1 "original" } }
