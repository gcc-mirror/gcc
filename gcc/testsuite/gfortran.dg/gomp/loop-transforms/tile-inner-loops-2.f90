! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

subroutine test2
  !$omp parallel do
  !$omp tile sizes(3,3)
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
end subroutine test2

! One gimple_omp_for should cover the outer two loops, another the inner two loops
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait tile sizes\(3, 3\)@0\n +for \(i = 0; i <= 100; i = i \+ 1\)\n +for \(j = -300; j <= 100; j = j \+ 1\)\n} 1 "original" } }
! { dg-final { scan-tree-dump-times {\#pragma omp loop_transform tile sizes\(3, 3\)@0\n +for \(k = -300; k <= 100; k = k \+ 1\)\n +for \(l = 0; l <= 100; l = l \+ 1\)} 1 "original" } }
! Collapse after the transformations should be 1
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait\n +for \(.omp_tile_index.\d = 0; .omp_tile_index.\d < 101; .omp_tile_index.\d = .omp_tile_index.\d \+ \d\)} 1 "omp_transform_loops" } }
