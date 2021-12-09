! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! { dg-final { scan-tree-dump-times "#pragma omp atomic relaxed" 4 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic release" 4 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture relaxed" 4 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture release" 2 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic read acquire" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst" 7 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic read seq_cst" 3 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture seq_cst" 6 "original" } }

subroutine foo ()
  integer :: x, v
  !$omp atomic
  i = i + 2

  !$omp atomic relaxed
  i = i + 2

  !$omp atomic seq_cst read
  v = x
  !$omp atomic seq_cst, read
  v = x
  !$omp atomic seq_cst write
  x = v
  !$omp atomic seq_cst ,write
  x = v
  !$omp atomic seq_cst update
  x = x + v
  !$omp atomic seq_cst , update
  x = x + v
  !$omp atomic seq_cst capture
  x = x + 2
  v = x
  !$omp end atomic
  !$omp atomic seq_cst, capture
  x = x + 2
  v = x
  !$omp end atomic
  !$omp atomic read , seq_cst
  v = x
  !$omp atomic write ,seq_cst
  x = v
  !$omp atomic update, seq_cst
  x = x + v
  !$omp atomic capture, seq_cst
  x = x + 2
  v = x
  !$omp end atomic
end

subroutine bar
  integer :: i, v
  real :: f
  !$omp atomic release, hint (0), update
  i = i + 1
  !$omp end atomic
  !$omp atomic hint(0)seq_cst
  i = i + 1
  !$omp atomic relaxed,update,hint (0)
  i = i + 1
  !$omp atomic release
  i = i + 1
  !$omp atomic relaxed
  i = i + 1
  !$omp atomic relaxed capture
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic capture,release , hint (1)
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic hint(0),relaxed capture
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic read acquire
  v = i
  !$omp atomic release,write
  i = v
  !$omp atomic hint(1),update,release
  f = f + 2.0
end

subroutine openmp51_foo
  integer :: x, v
  !$omp atomic update seq_cst capture
  x = x + 2
  v = x
  !$omp end atomic
  !$omp atomic seq_cst, capture, update
  x = x + 2
  v = x
  !$omp end atomic
  !$omp atomic capture, seq_cst ,update
  x = x + 2
  v = x
  !$omp end atomic
end

subroutine openmp51_bar
  integer :: i, v
  real :: f
  !$omp atomic relaxed capture update
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic update capture,release , hint (1)
  i = i + 1
  v = i
  !$omp end atomic
  !$omp atomic hint(0),update relaxed capture
  i = i + 1
  v = i
  !$omp end atomic
end
