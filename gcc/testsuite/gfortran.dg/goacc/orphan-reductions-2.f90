! Verify that we diagnose "gang reduction on an orphan loop" for automatically
! assigned gang level of parallelism.

! { dg-do compile }
! { dg-additional-options "-fopt-info-optimized-omp" }
! { dg-additional-options "-Wopenacc-parallelism" }

subroutine s1
  implicit none
  !$acc routine gang
  ! { dg-bogus "\[Ww\]arning: region is worker partitioned but does not contain worker partitioned code" "TODO default 'gang' 'vector'" { xfail *-*-* } .-3 }
  !TODO It's the compiler's own decision to not use 'worker' parallelism here, so it doesn't make sense to bother the user about it.
  integer i, sum

  sum = 0
  ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC gang vector loop parallelism" }
  do i = 1, 10
     sum = sum + 1
  end do
end subroutine s1

subroutine s2
  implicit none
  !$acc routine gang
  integer i, j, sum

  sum = 0
  ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC gang worker loop parallelism" }
  do i = 1, 10
     !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
     do j = 1, 10
        sum = sum + 1
     end do
  end do
end subroutine s2

subroutine s3
  implicit none
  !$acc routine gang
  integer i, j, k, sum

  sum = 0
  ! { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 }
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, 10
     !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC worker loop parallelism" }
     do j = 1, 10
        !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
        do k = 1, 10
           sum = sum + 1
        end do
     end do
  end do
end subroutine s3

subroutine s4
  implicit none

  integer i, j, k, sum

  sum = 0
  !$acc parallel copy(sum)
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC gang vector loop parallelism" }
  do i = 1, 10
     sum = sum + 1
  end do
  !$acc end parallel

  !$acc parallel copy(sum)
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC gang worker loop parallelism" }
  do i = 1, 10
     !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
     do j = 1, 10
        sum = sum + 1
     end do
  end do
  !$acc end parallel

  !$acc parallel copy(sum)
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, 10
     !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC worker loop parallelism" }
     do j = 1, 10
        !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
        do k = 1, 10
           sum = sum + 1
        end do
     end do
  end do
  !$acc end parallel
end subroutine s4
