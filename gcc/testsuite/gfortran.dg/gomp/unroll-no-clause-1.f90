! { dg-additional-options "-O2 -fdump-tree-gimple" }

subroutine test
  !$omp unroll
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

! { dg-final { scan-tree-dump-times "\.ANNOTATE \\\(\[^\n\r\]*, 1, 8\\\);" 2 "gimple" } }
