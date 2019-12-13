! Test erroneous cases of nested reduction loops.

subroutine acc_parallel ()
  implicit none (type, external)
  integer :: i, j, k, l, sum, diff

  !$acc parallel
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
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
  !$acc end parallel
end subroutine acc_parallel

! The same tests as above, but using a combined parallel loop construct.

subroutine acc_parallel_loop ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc parallel loop
  do h = 1, 10
    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(+:sum)  ! { dg-warning "nested loop in reduction needs reduction clause for .diff." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
! the parallel region, not the outermost loop.

subroutine acc_parallel_reduction ()
  implicit none (type, external)
  integer :: i, j, k, l, sum, diff

  !$acc parallel reduction(+:sum)
    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
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
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
  !$acc end parallel
end subroutine acc_parallel_reduction

! The same tests as above, but using a combined parallel loop construct, and
! the outermost reduction clause is on that one, not the outermost loop.  */
subroutine acc_parallel_loop_reduction ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc parallel loop reduction(+:sum)
  do h = 1, 10
    !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
    do i = 1, 10
      !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop  ! { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)  ! { dg-warning "conflicting reduction operations for .sum." }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do

      !$acc loop reduction(+:sum)  ! { dg-warning "nested loop in reduction needs reduction clause for .diff." }
      ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do j = 1, 10
        !$acc loop reduction(-:diff)
        do k = 1, 10
          diff = 1
        end do
      end do
    end do
  end do
end subroutine acc_parallel_loop_reduction

! The same tests as above, but inside a routine construct.
subroutine acc_routine ()
  implicit none (type, external)
  !$acc routine gang
  integer :: i, j, k, l, sum, diff

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
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
end subroutine acc_routine

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
