! Test erroneous cases of nested 'reduction' clauses.

! See also 'c-c++-common/goacc/nested-reductions-2-kernels.c'.

subroutine acc_kernels ()
  integer :: i, j, k, sum, diff

  ! FIXME:  No diagnostics are produced for these loops because reductions
  ! in kernels regions are not supported yet.
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
      !$acc loop
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:diff)
      do j = 1, 10
        !$acc loop
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do
  !$acc end kernels
end subroutine acc_kernels
