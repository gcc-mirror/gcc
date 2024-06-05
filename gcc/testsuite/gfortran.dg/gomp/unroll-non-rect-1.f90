subroutine test
  implicit none

  integer :: i, j, k
  !$omp unroll full
  do i = -3, 5
    do j = 1,10
      do k = j,j*2 + 1
        call dummy (i)
      end do
    end do
  end do
end subroutine
