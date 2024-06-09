subroutine test3
  implicit none
  integer :: i, j, k

  !$omp taskloop collapse(3)
  !$omp tile sizes (1,2) ! { dg-error "TILE construct at \\\(1\\\) generates 2 loops with canonical form but 3 loops are needed" }
  !$omp tile sizes (1,2)
  do i = 1,100
    do j = 1,100
      call dummy(j)
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
  !$omp end taskloop
end subroutine test3
