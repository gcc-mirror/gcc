! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

subroutine test3
  !$omp parallel do
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
end subroutine test3

! gimple_omp_for collapse should be extended to cover all loops affected by the transformations (i.e. 4)
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait tile sizes\(3, 3, 3\)@0 tile sizes\(3, 3\)@2\n +for \(i = 0; i <= 100; i = i \+ 1\)\n +for \(j = -300; j <= 100; j = j \+ 1\)\n +for \(k = -300; k <= 100; k = k \+ 1\)\n +for \(l = 0; l <= 100; l = l \+ 1\)} 1 "original" } }
! Collapse after the transformations should be 1
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait private\(l.0\) private\(k\)\n +for \(.omp_tile_index.\d = 0; .omp_tile_index.\d < 101; .omp_tile_index.\d = .omp_tile_index.\d \+ \d\)} 1 "omp_transform_loops" } }
