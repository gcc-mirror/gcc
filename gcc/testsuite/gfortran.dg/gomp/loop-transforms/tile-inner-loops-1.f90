! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

subroutine test1
  !$omp parallel do collapse(2)
  do i=0,100
     !$omp tile sizes(4)
     do j=-300,100
        call dummy (j)
     end do
  end do
end subroutine test1

! Collapse of the gimple_omp_for should be unaffacted by the transformation
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait collapse\(2\) tile sizes\(4\).1\n +for \(i = 0; i <= 100; i = i \+ 1\)\n +for \(j = -300; j <= 100; j = j \+ 1\)} 1 "original" } }
! { dg-final { scan-tree-dump-times {\#pragma omp for nowait collapse\(2\) private\(j.0\) private\(j\)\n +for \(i = 0; i < 101; i = i \+ 1\)\n +for \(.omp_tile_index.\d = -300; .omp_tile_index.\d < 101; .omp_tile_index.\d = .omp_tile_index.\d \+ 4\)} 1 "omp_transform_loops" } }
