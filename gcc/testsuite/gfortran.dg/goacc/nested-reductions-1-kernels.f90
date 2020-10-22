! Test cases of nested 'reduction' clauses expected to compile cleanly.

! See also 'c-c++-common/goacc/nested-reductions-1-kernels.c'.

subroutine acc_kernels ()
  implicit none (type, external)
  integer :: i, j, k, sum, diff

  ! FIXME:  These tests are not meaningful yet because reductions in
  ! kernels regions are not supported yet.
  !$acc kernels
    !$acc loop reduction(+:sum)
    do i = 1, 10
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do


    !$acc loop reduction(+:sum)
    do i = 1, 10
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do


    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

  !$acc end kernels
end subroutine acc_kernels
