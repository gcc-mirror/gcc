! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

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

! { dg-final { scan-tree-dump-times {#pragma omp for nowait unroll_partial\(1\) tile sizes\(8, 8\)} 1 "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp loop_transform unroll_partial" "omp_transform_loops" } }

! Tiling adds two floor and two tile loops.

! Number of conditional statements after tiling:
!     5
!  =  2    (lowering of 2 tile loops)
!  +  1    (partial tile handling in 2 tile loops)
!  +  1    (lowering of non-associated floor loop)

! The unrolling with unroll factor 1 currently gets executed (TODO could/should be skipped?)

! { dg-final { scan-tree-dump-times {if \([A-Za-z0-9_.]+ < } 5 "omp_transform_loops" } }
