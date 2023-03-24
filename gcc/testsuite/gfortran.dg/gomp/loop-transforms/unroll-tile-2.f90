! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

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

! { dg-final { scan-tree-dump-times {#pragma omp for nowait unroll_partial\(2\) tile sizes\(8, 8, 4\)} 1 "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp loop_transform unroll_partial" "omp_transform_loops" } }

! Check the number of loops

! Tiling adds three tile and three floor loops.
! The outermost floor loop is associated with the "!$omp parallel do"
! and hence it isn't lowered in the transformation pass.
! Number of conditional statements after tiling:
!     8
!  =  2    (inner floor loop lowering)
!  +  3    (partial tile handling in 3 tile loops)
!  +  3    (lowering of 3 tile loops)
!
! Unrolling creates 2 copies of the tiled loop nest.

! { dg-final { scan-tree-dump-times {if \([A-Za-z0-9_.]+ < } 16 "omp_transform_loops" } }
