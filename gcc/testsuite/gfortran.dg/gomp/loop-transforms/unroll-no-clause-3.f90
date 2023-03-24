! { dg-additional-options "--param=omp-unroll-full-max-iterations=10" }
! { dg-additional-options "--param=omp-unroll-default-factor=10" }
! { dg-additional-options "-fopt-info-optimized -fdump-tree-omp_transform_loops-details" }

subroutine test
  !$omp unroll ! { dg-optimized {added 'partial\(10\)' clause to 'omp unroll' directive} }
  do i = 1,20
     do j = 1,10
        call dummy3(i,j)
     end do
  end do
  !$omp end unroll

  !$omp unroll ! { dg-optimized {added 'partial\(10\)' clause to 'omp unroll' directive} }
  do i = 1,21
  !$omp unroll ! { dg-optimized {assigned 'full' clause to 'omp unroll' with small constant number of iterations} }
     do j = 1,6
        call dummy3(i,j)
     end do
  end do
  !$omp end unroll
end subroutine test

