! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

subroutine test3
  !$omp parallel do
  !$omp tile sizes(3)
  do i=0,100
     do j=-300,100
        !$omp tile sizes(3,3)
        do k=-300,100
           do l=0,100
              call dummy (l)
           end do
        end do
     end do
  end do
end subroutine test3

! The outer gimple_omp_for should not cover the loop with the tile transformation
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait tile sizes\(3\)@0\n +for \(i = 0; i <= 100; i = i \+ 1\)\n} 1 "original" } }
! { dg-final { scan-tree-dump-times {\#pragma omp loop_transform tile sizes\(3, 3\)@0\n +for \(k = -300; k <= 100; k = k \+ 1\)\n +for \(l = 0; l <= 100; l = l \+ 1\)} 1 "original" } }


! After transformations, the outer loop should be a floor loop created
! by the tiling and the outer construct type and non-transformation
! clauses should be unaffected by the tiling
! { dg-final { scan-tree-dump {\#pragma omp for nowait\n +for \(.omp_tile_index.\d = 0; .omp_tile_index.\d < 101; .omp_tile_index.\d = .omp_tile_index.\d \+ 3\)} "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {\#pragma omp} 2 "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {\#pragma omp parallel} 1 "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {\#pragma omp for} 1 "omp_transform_loops" } }
