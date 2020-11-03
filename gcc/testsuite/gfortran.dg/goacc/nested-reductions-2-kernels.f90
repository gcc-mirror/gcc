! Test erroneous cases of nested 'reduction' clauses.

! See also 'c-c++-common/goacc/nested-reductions-2-kernels.c'.

subroutine acc_kernels ()
  implicit none (type, external)
  integer :: i, j, k, l, sum, diff

  !$acc kernels
    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop collapse(2)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      do j = 1, 10
        do k = 1, 10
          !$acc loop reduction(+:sum)
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(+:sum)
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      do j = 1, 10
        !$acc loop reduction(-:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(*:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(*:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(-:diff)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(+:sum)  ! { dg-warning "nested loop in reduction needs reduction clause for .diff." }
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
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop collapse(2)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        do k = 1, 10
          !$acc loop reduction(+:sum)
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do


    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(+:sum)
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(-:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(*:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(*:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(+:sum) reduction(-:diff)
    do i = 1, 10
      !$acc loop reduction(-:diff)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(+:sum)  ! { dg-warning "nested loop in reduction needs reduction clause for .diff." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
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
! the kernels region, not the outermost loop.

subroutine acc_kernels_reduction ()
  implicit none (type, external)

  ! In contrast to the 'parallel' construct, the 'reduction' clause is not
  ! supported on the 'kernels' construct.
end subroutine acc_kernels_reduction

! The same tests as above, but using a combined kernels loop construct, and
! the outermost reduction clause is on that one, not the outermost loop.  */
subroutine acc_kernels_loop_reduction ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc kernels loop reduction(+:sum)
  do h = 1, 10
    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop collapse(2)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        do k = 1, 10
          !$acc loop reduction(+:sum)
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(+:sum)
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }  
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(-:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    !$acc loop reduction(max:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(*:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(max:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
    do i = 1, 10
      !$acc loop reduction(-:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
        do k = 1, 10
          !$acc loop reduction(*:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
          do l = 1, 10
            sum = 1
          end do
        end do
      end do
    end do

    !$acc loop reduction(-:diff)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop reduction(-:diff)  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(+:sum)  ! { dg-warning "nested loop in reduction needs reduction clause for .diff." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "TODO" { xfail *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  end do
end subroutine acc_kernels_loop_reduction
