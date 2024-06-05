subroutine test
  implicit none
  integer :: i, j, k, l, m, n, o
  !$omp do private (i, j, k, l)
  !$omp tile sizes(2, 3)
  !$omp tile sizes(3, 4, 5)
  !$omp tile sizes(6, 7, 8, 9)
  do i = 1, 100
    do j = 1, 100
      do k = 1, 100
        do l = 1, 100
          call dummy(i)
        end do
      end do
    end do
  end do

  !$omp do private (i, j, k, l, m, n)
  !$omp tile sizes(2, 3)
  do i = 1, 100
    !$omp tile sizes(3, 4, 5)
    do j = 1, 100
      !$omp tile sizes(6, 7, 8, 9)
      do k = 1, 100
        do l = 1, 100
          do m = 1, 100
            !$omp unroll partial(2)
            do n = 1, 100
              call dummy(i)
            end do
          end do
        end do
      end do
    end do
  end do

  !$omp do collapse(2) private (i, j, k, l, m)
  do i = 1, 100
    !$omp tile sizes(2, 3)
    !$omp tile sizes(3, 4, 5)
    !$omp tile sizes(6, 7, 8, 9)
    do j = 1, 100
      do k = 1, 100
        do l = 1, 100
          do m = 1, 100
            call dummy(i)
          end do
        end do
      end do
    end do
  end do

  !$omp do private (i, j, k, l, m, n, o) collapse(2)
  do i = 1, 100
    !$omp tile sizes(2, 3)
    do j = 1, 100
      !$omp tile sizes(3, 4, 5)
      do k = 1, 100
        !$omp tile sizes(6, 7, 8, 9)
        do l = 1, 100
          do m = 1, 100
            do n = 1, 100
              !$omp unroll partial(2)
              do o = 1, 100
                call dummy(i)
              end do
            end do
          end do
        end do
      end do
    end do
  end do
end subroutine test
