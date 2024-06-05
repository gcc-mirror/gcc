! It isn't really clear what is supposed to be valid and what isn't when mixing
! imperfectly nested loops with generated loops.  Sorry for now until that is
! clarified.

subroutine bar
  integer :: i, j
  !$omp do collapse(2)
  do i = 0, 31
    call foo (i, -1)
    !$omp tile sizes (2)	! { dg-message "Imperfectly nested loop using generated loops" }
    do j = 0, 31
      call foo (i, j)
    end do
    call foo (i, -2)
  end do
end subroutine bar

subroutine baz
  integer :: i, j, k, l
  !$omp do collapse(2)
  do i = 0, 31
    call foo (i, -1)
    !$omp tile sizes (2, 2)	! { dg-message "Imperfectly nested loop using generated loops" }
    do j = 0, 31
      !$omp tile sizes (2, 2)
      do k = 0, 31
        do l = 0, 31
          call foo (i + k, j + l)
        end do
      end do
    end do
    call foo (i, -2)
  end do
end subroutine baz

subroutine qux
  integer :: i, j, k, l, m
  !$omp do collapse(2)
  do i = 0, 31
    m = i + 6
    call foo (i, -1)
    !$omp tile sizes (2)	! { dg-message "Imperfectly nested loop using generated loops" }
    do j = m, 31
      call foo (i, j)
    end do
    call foo (i, -2)
  end do
end subroutine qux

subroutine freddy
  integer :: i, j, k, l, m
  !$omp do collapse(2)
  do i = 0, 31
    block
      integer :: m
      m = i + 6
      call foo (i, -1)
      !$omp tile sizes (2, 2)	! { dg-message "Imperfectly nested loop using generated loops" }
      do j = 0, 31
        !$omp tile sizes (2, 2)
        do k = 0, 31
          do l = m, 31
            call foo (i + k, j + l)
          end do
        end do
      end do
      call foo (i, -2)
    end block
  end do
end subroutine freddy
