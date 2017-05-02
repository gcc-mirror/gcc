! Ensure that the middle end does not assign gang level parallelism to
! orphan loop containing reductions.

! { dg-do compile }
! { dg-additional-options "-fopt-info-optimized-omp" }
! { dg-additional-options "-Wopenacc-parallelism" }

subroutine s1 ! { dg-warning "region is gang partitioned but does not contain gang partitioned code" }
  implicit none
  !$acc routine gang
  integer i, sum

  sum = 0
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
  do i = 1, 10
     sum = sum + 1
  end do
end subroutine s1

subroutine s2 ! { dg-warning "region is gang partitioned but does not contain gang partitioned code" }
  implicit none
  !$acc routine gang
  integer i, j, sum

  sum = 0
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC worker loop parallelism" }
  do i = 1, 10
     !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
     do j = 1, 10
        sum = sum + 1
     end do
  end do
end subroutine s2

subroutine s3 ! { dg-warning "region is gang partitioned but does not contain gang partitioned code" }
  implicit none
  !$acc routine gang
  integer i, j, k, sum

  sum = 0
  !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC worker loop parallelism" }
  do i = 1, 10
     !$acc loop reduction (+:sum) ! { dg-optimized "assigned OpenACC seq loop parallelism" }
     ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
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
