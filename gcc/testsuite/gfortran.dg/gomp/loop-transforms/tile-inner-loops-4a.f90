! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

subroutine test3
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

! There should be separate gimple_omp_for constructs for the tile constructs because the tiling depth
! of the outer construct does not reach the level of the inner construct
! { dg-final { scan-tree-dump-times {\#pragma omp loop_transform tile sizes\(3\)@0\n +for \(i = 0; i <= 100; i = i \+ 1\)\n} 1 "original" } }
! { dg-final { scan-tree-dump-times {\#pragma omp loop_transform tile sizes\(3, 3\)@0\n +for \(k = -300; k <= 100; k = k \+ 1\)\n +for \(l = 0; l <= 100; l = l \+ 1\)} 1 "original" } }


! The loops should be lowered after the tiling transformations
! { dg-final { scan-tree-dump-not {\#pragma omp} "omp_transform_loops" } }
! { dg-final { scan-tree-dump-times {if \(.omp_tile_index} 3 "omp_transform_loops" } }
