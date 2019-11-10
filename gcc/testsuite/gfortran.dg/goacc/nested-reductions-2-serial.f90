! Test erroneous cases of nested 'reduction' clauses.

! See also 'c-c++-common/goacc/nested-reductions-2-serial.c'.

! { dg-additional-options -Wuninitialized }

subroutine acc_serial ()
  implicit none (type, external)
  integer :: i, j, k, l, sum, diff

  !$acc serial
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
  !$acc end serial
end subroutine acc_serial

! The same tests as above, but using a combined serial loop construct.

subroutine acc_serial_loop ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc serial loop
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
end subroutine acc_serial_loop

! The same tests as above, but now the outermost reduction clause is on
! the serial region, not the outermost loop.

subroutine acc_serial_reduction ()
  implicit none (type, external)
  integer :: i, j, k, l, sum, diff

  !$acc serial reduction(+:sum)
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
  !$acc end serial
end subroutine acc_serial_reduction

! The same tests as above, but using a combined serial loop construct, and
! the outermost reduction clause is on that one, not the outermost loop.  */
subroutine acc_serial_loop_reduction ()
  implicit none (type, external)
  integer :: h, i, j, k, l, sum, diff

  !$acc serial loop reduction(+:sum)
  ! implicit 'copy (sum, diff)'
  ! { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
  ! { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 }
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
end subroutine acc_serial_loop_reduction
