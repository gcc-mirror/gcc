subroutine test1
  !$omp tile sizes(1)
  do i = 1,100
    do j = 1,i
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
end subroutine test1

subroutine test5
  !$omp tile sizes(1,2)
  do i = 1,100
    do j = 1,100
      do k = 1,j
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
end subroutine test5
