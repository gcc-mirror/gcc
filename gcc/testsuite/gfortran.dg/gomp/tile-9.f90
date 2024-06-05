subroutine test1
  integer :: i, j, k, l
  !$omp do collapse(4) private (i, j, k, l)
  do i = 0, 1023 ! { dg-error "cannot be redefined inside loop" }
    !$omp tile sizes (2, 2, 2)
    do j = 0, 1023
      !$omp tile sizes (3, 3)
      do k = 0, 1023
        !$omp tile sizes (4)
        do i = 0, 1023 ! { dg-error "!\\\$OMP DO iteration variable used in more than one loop" }
        end do ! { dg-error "cannot be redefined inside loop" "" { target *-*-* } .-1 }
      end do
    end do
  end do
end subroutine test1

subroutine test2
  integer(kind=8) :: i
  integer :: j, k, l
  !$omp do collapse(4) private (i, j, k, l)
  do i = 0, 1023
    !$omp tile sizes (2, 2, 2)
    do j = 0, 1023
      !$omp tile sizes (3, 3)
      do k = 0, 1023
        !$omp tile sizes (4)
        do l = i, 1023 ! { dg-error "!\\\$OMP DO loop start expression not in canonical form" }
        end do
      end do
    end do
  end do
end subroutine test2

subroutine test3
  integer :: i, j, k, l
  !$omp do collapse(4) private (i, j, k, l)
  do i = 0, 1023
    !$omp tile sizes (2, 2, 2)
    do j = 0, 1023
      !$omp tile sizes (3, 3)
      do k = 0, 1023
        !$omp tile sizes (4)
        do l = 0, 7 * i * i ! { dg-error "!\\\$OMP DO loop end expression not in canonical form" }
        end do
      end do
    end do
  end do
end subroutine test3

subroutine test4
  integer :: i, j, k, l
  !$omp do collapse(4) private (i, j, k, l)
  do i = 0, 1023
    !$omp tile sizes (2, 2, 2)
    do j = 0, 1023
      !$omp tile sizes (3, 3)
      do k = 0, 1023
        !$omp tile sizes (4)
        do l = i * i, 1023 ! { dg-error "!\\\$OMP DO loop start expression not in canonical form" }
        end do
      end do
    end do
  end do
end subroutine test4

subroutine test5
  integer :: i, j, k, l
  !$omp do collapse(4) private (i, j, k, l)
  do i = 0, 1023
    !$omp tile sizes (2, 2, 2)
    do j = 0, 1023
      !$omp tile sizes (3, 3)
      do k = 0, 1023
        !$omp tile sizes (4)
        do l = 0, 1023, j ! { dg-error "!\\\$OMP TILE loop increment not in canonical form" }
        end do
      end do
    end do
  end do
end subroutine test5

subroutine test6
  integer :: i, j, k, l
  !$omp do collapse(4) private (i, j, k, l)
  do i = 0, 1023
    !$omp tile sizes (2, 2, 2)
    do j = 0, 1023
      !$omp tile sizes (3, 3)
      do k = 0, 1023
        !$omp tile sizes (4)
        do l = 0, i - 2 ! { dg-message "Non-rectangular loops from generated loops unsupported" }
        end do
      end do
    end do
  end do
end subroutine test6
