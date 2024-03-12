! Test that various errors involving references to variables bound
! in intervening code in the DO loop control expressions are diagnosed.

subroutine foo (x, y)
  integer :: x, y
end subroutine

subroutine f1 ()
  integer :: i, j

  !$omp do collapse (2)
  do i = 1, 64
    block
      integer :: v
      v = (i + 4) * 2
      do j = v, 64  ! { dg-error "loop start expression at .1. uses variable bound in intervening code" }
        call foo (i, j)
      end do
    end block
  end do
end subroutine

subroutine f2 ()
  integer :: i, j

  !$omp do collapse (2)
  do i = 1, 64
    block
      integer :: v
      v = (i + 4) * 2
      do j = 1, v  ! { dg-error "loop end expression at .1. uses variable bound in intervening code" }
        call foo (i, j)
      end do
    end block
  end do
end subroutine

subroutine f3 ()
  integer :: i, j

  !$omp do collapse (2)
  do i = 1, 64
    block
      integer :: v
      v = (i + 4) * 2
      do j = 1, 64, v  ! { dg-error "loop increment expression at .1. uses variable bound in intervening code" }
        call foo (i, j)
      end do
    end block
  end do
end subroutine

subroutine f4 ()
  integer :: i

  !$omp do collapse (2)
  do i = 1, 64
    block
      integer :: j
      do j = 1, 64  ! { dg-error "iteration variable at .1. is bound in intervening code" }
        call foo (i, j)
      end do
    end block
  end do
end subroutine

subroutine f5 ()
  integer :: i

  !$omp do collapse (2)
  do i = 1, 64
    block
      integer :: j
      integer :: v
      v = (i + 4) * 2
      do j = v, 64  ! { dg-error "iteration variable at .1. is bound in intervening code" }
        call foo (i, j)
      end do
    end block
  end do
end subroutine
