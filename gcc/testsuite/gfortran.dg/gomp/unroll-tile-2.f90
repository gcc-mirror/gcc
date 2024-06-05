! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

function mult (a, b) result (c)
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner

  allocate(c( n, m ))
  c = 0

  !$omp target
  !$omp parallel do
  !$omp unroll partial(2)
  !$omp tile sizes (8,8,4)
  do i = 1,m
    do j = 1,n
      do k = 1, n
        c(j,i) = c(j,i) + a(k, i) * b(j, k)
      end do
    end do
  end do
  !$omp end target
end function mult

! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp unroll partial\\\(2\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp tile sizes\\\(8, 8, 4\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp unroll" "gimple" } }
! { dg-final { scan-tree-dump-not "#pragma omp tile" "gimple" } }

! Check the number of loops

! Tiling adds three tile and three floor loops.
! The outermost tile loop is then partially unrolled, turning it
! into one tile and one floor loop, so now 7 loops in total, one
! of them being fully unrolled.  And finally the outermost loop is
! associated with the !$omp parallel do and so not lowered during
! gimplification.

! { dg-final { scan-tree-dump-times "if \\\(\[A-Za-z0-9_.\]+ <" 5 "gimple" } }
! { dg-final { scan-tree-dump-times "\.ANNOTATE \\\(\[^\n\r\]*, 1, 2\\\);" 1 "gimple" } }
