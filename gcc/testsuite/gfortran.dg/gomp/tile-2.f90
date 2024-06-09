subroutine test1
  implicit none
  integer :: i, j, k

  !$omp tile sizes (1,2)
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

  !$omp tile sizes (8)
  !$omp tile sizes (1,2)
  !$omp tile sizes (1,2,3)
  do i = 1,100
    do j = 1,100
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
end subroutine test1

subroutine test2
  implicit none
  integer :: i, j, k

  !$omp taskloop collapse(2)
  !$omp tile sizes (3,4)
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

  !$omp taskloop simd
  !$omp tile sizes (8)
  !$omp tile sizes (1,2)
  !$omp tile sizes (1,2,3)
  do i = 1,100
    do j = 1,100
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
  !$omp end taskloop simd
end subroutine test2
