! Test erroneous cases of nested 'reduction' clauses.

! See also 'c-c++-common/goacc/nested-reductions-2-routine.c'.

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
