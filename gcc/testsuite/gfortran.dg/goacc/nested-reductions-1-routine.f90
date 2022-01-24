! Test cases of nested 'reduction' clauses expected to compile cleanly.

! See also 'c-c++-common/goacc/nested-reductions-1-routine.c'.

! { dg-additional-options -Wuninitialized }

subroutine acc_routine ()
  implicit none (type, external)
  !$acc routine gang

  integer :: i, j, k, sum, diff

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
    !$acc loop reduction(+:sum)
    ! { dg-warning {'sum' is used uninitialized} {} { target *-*-* } .-1 }
    do i = 1, 10
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
    !$acc loop collapse(2) reduction(+:sum)
    do i = 1, 10
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop reduction(+:sum)
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
    !$acc loop reduction(+:sum)
    do i = 1, 10
      !$acc loop collapse(2) reduction(+:sum)
      do j = 1, 10
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
    !$acc loop reduction(+:sum)
    do i = 1, 10
      do j = 1, 10
        !$acc loop reduction(+:sum)
        do k = 1, 10
          sum = 1
        end do
      end do
    end do

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
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

    ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
    !$acc loop reduction(+:sum) reduction(-:diff)
    ! { dg-warning {'diff' is used uninitialized} {} { target *-*-* } .-1 }
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
end subroutine acc_routine
