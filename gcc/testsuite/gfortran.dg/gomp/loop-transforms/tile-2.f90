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
  !$end omp tile

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
  !$end omp tile
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
  !$end omp tile
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
  !$end omp tile
  !$omp end taskloop simd
end subroutine test2

subroutine test3
  implicit none
  integer :: i, j, k

  !$omp taskloop collapse(3) ! { dg-error {not enough DO loops for collapsed \!\$OMP TASKLOOP at \(1\)} }
  !$omp tile sizes (1,2) ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TASKLOOP} }
  !$omp tile sizes (1,2)
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
  !$omp end taskloop
end subroutine test3
