! { dg-additional-options "-O2 -fdump-tree-original -fdump-tree-gimple" }

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

! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp tile sizes\\\(3, 3\\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait\[\n\r\]+ +for \\\(i.\\\d = 0; i.\\\d <= 100; i.\\\d = i.\\\d \\\+ 3\\\)" 1 "gimple" } }
