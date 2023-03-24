! { dg-additional-options "-fopt-info-optimized -fdump-tree-omp_transform_loops-details" }

subroutine test
  !$omp unroll ! { dg-optimized {assigned 'full' clause to 'omp unroll' with small constant number of iterations} }
  do i = 1,5
     do j = 1,10
        call dummy3(i,j)
     end do
  end do
  !$omp end unroll

  !$omp unroll
  do i = 1,6
     do j = 1,6
        call dummy3(i,j)
     end do
  end do
  !$omp end unroll
end subroutine test

