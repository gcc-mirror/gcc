! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

function mult (a, b) result (c)
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner

  allocate(c( n, m ))

  !$omp parallel do
  !$omp unroll partial(1)
  !$omp tile sizes (8,8)
  do i = 1,m
    do j = 1,n
      inner = 0
      do k = 1, n
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end function mult

! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp unroll partial\\\(1\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp tile sizes\\\(8, 8\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp unroll" "gimple" } }
! { dg-final { scan-tree-dump-not "#pragma omp tile" "gimple" } }

! Tiling adds two floor and two tile loops.
! Unroll with partial(1) is effectively ignored and the innermost
! loop isn't associated with anything.  So that means 5 loops,
! with the outermost associated with !$omp parallel do, where
! the innermost loop gimplifies condition into a boolean temporary.

! { dg-final { scan-tree-dump-times "if \\\(\[A-Za-z0-9_.\]+ <" 3 "gimple" } }
