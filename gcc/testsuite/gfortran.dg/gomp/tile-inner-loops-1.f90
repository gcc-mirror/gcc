! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

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
! { dg-final { scan-tree-dump-times "#pragma omp for nowait collapse\\\(2\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp tile sizes\\\(4\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait collapse\\\(2\\\)\[\n\r\]+ +for \\\(i = 0; i <= 100; i = i \\\+ 1\\\)\[\n\r\]+ +for \\\(j.\\\d = -300; j.\\\d <= 100; j.\\\d = j.\\\d \\\+ 4\\\)" 1 "gimple" } }
