! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

subroutine test
  !$omp tile sizes(3,3,3)
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
end subroutine test

! gimple_omp_for collapse should be extended to cover all loops affected by the transformations (i.e. 4)
! { dg-final { scan-tree-dump-times {\#pragma omp loop_transform tile sizes\(3, 3, 3\)@0 tile sizes\(3, 3\)@2\n +for \(i = 0; i <= 100; i = i \+ 1\)\n +for \(j = -300; j <= 100; j = j \+ 1\)\n +for \(k = -300; k <= 100; k = k \+ 1\)\n +for \(l = 0; l <= 100; l = l \+ 1\)} 1 "original" } }

! The loops should be lowered after the tiling transformations
! { dg-final { scan-tree-dump-not {\#pragma omp} "omp_transform_loops" } }

! Third level is tiled first by the inner construct. The resulting floor loop is tiled by the outer construct.
! { dg-final { scan-tree-dump-times {if \(.omp_tile_index.1} 2 "omp_transform_loops" } }

! All other levels are tiled once
! { dg-final { scan-tree-dump-times {if \(.omp_tile_index.2} 1 "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {if \(.omp_tile_index.3} 1 "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {if \(.omp_tile_index.4} 1 "omp_transform_loops" } }

