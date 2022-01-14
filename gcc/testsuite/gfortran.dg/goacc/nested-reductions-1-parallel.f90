! Test cases of nested 'reduction' clauses expected to compile cleanly.

! See also 'c-c++-common/goacc/nested-reductions-1-parallel.c'.

! { dg-additional-options -Wuninitialized }

subroutine acc_parallel ()
  implicit none (type, external)
  integer :: i, j, k, sum, diff

  !$acc parallel
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
  !$acc end parallel
end subroutine acc_parallel

! The same tests as above, but using a combined parallel loop construct.

subroutine acc_parallel_loop ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc parallel loop
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  end do
end subroutine acc_parallel_loop

! The same tests as above, but now the outermost reduction clause is on
! the parallel region, not the outermost loop.  */

subroutine acc_parallel_reduction ()
  implicit none (type, external)
  integer :: i, j, k, sum, diff

  !$acc parallel reduction(+:sum)
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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

    !$acc loop reduction(+:sum)
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

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  !$acc end parallel
end subroutine acc_parallel_reduction

! The same tests as above, but using a combined parallel loop construct, and
! the outermost reduction clause is on that one, not the outermost loop.  */
subroutine acc_parallel_loop_reduction ()
  implicit none (type, external)
  integer :: h, i, j, k, sum, diff

  !$acc parallel loop reduction(+:sum)
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(-:diff)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop  ! { dg-warning "insufficient partitioning available to parallelize loop" }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  end do
end subroutine acc_parallel_loop_reduction
