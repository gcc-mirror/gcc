! Test cases of nested 'reduction' clauses expected to compile cleanly.

! See also 'c-c++-common/goacc/nested-reductions-1-kernels.c'.

subroutine acc_kernels ()
  implicit none (type, external)
  integer :: i, j, k, sum, diff

  !$acc kernels
    !$acc loop reduction(+:sum)
    do i = 1, 10
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop collapse(2) reduction(+:sum)
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
      !$acc loop collapse(2) reduction(+:sum)
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

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(+:sum)
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  !$acc end kernels
end subroutine acc_kernels

! The same tests as above, but using a combined kernels loop construct.

subroutine acc_kernels_loop ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc kernels loop
  do h = 1, 10
    !$acc loop reduction(+:sum)
    do i = 1, 10
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop collapse(2) reduction(+:sum)
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
      !$acc loop collapse(2) reduction(+:sum)
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
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  end do
end subroutine acc_kernels_loop

! The same tests as above, but now the outermost reduction clause is on
! the kernels region, not the outermost loop.  */

subroutine acc_kernels_reduction ()
  implicit none (type, external)

  ! In contrast to the 'parallel' construct, the 'reduction' clause is not
  ! supported on the 'kernels' construct.
end subroutine acc_kernels_reduction

! The same tests as above, but using a combined kernels loop construct, and
! the outermost reduction clause is on that one, not the outermost loop.  */
subroutine acc_kernels_loop_reduction ()
  implicit none (type, external)
  integer :: h, i, j, k, sum, diff

  !$acc kernels loop reduction(+:sum)
  do h = 1, 10
    do i = 1, 10
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

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
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    do i = 1, 10
      do j = 1, 10
        !$acc loop
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop  ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  end do
end subroutine acc_kernels_loop_reduction
