subroutine test
  implicit none
  integer :: i, j, k

  !$omp tile sizes ( 1 )
  do i = 1,100
    call dummy(i)
  end do

  !$omp tile sizes(1)
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes(2+3)
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes(1,2)
  do i = 1,100
    do j = 1,100
      call dummy(j)
    end do
  end do
  !$omp end tile

  !$omp tile sizes(1,2,1)
  do i = 1,100
    do j = 1,100
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
end subroutine test
