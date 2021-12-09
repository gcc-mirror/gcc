! Verify that the error message for gang reductions on orphaned OpenACC loops
! is not reported for non-orphaned loops.

! { dg-additional-options "-Wopenacc-parallelism" }

subroutine kernels
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc kernels
  !$acc loop gang reduction(+:sum) ! { dg-bogus "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end kernels
end subroutine kernels

subroutine parallel
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc parallel
  !$acc loop gang reduction(+:sum) ! { dg-bogus "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end parallel
end subroutine parallel

subroutine serial
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc serial ! { dg-warning "region contains gang partitioned code but is not gang partitioned" }
  !$acc loop gang reduction(+:sum) ! { dg-bogus "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end serial
end subroutine serial

subroutine kernels_combined
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc kernels loop gang reduction(+:sum) ! { dg-bogus "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do
end subroutine kernels_combined

subroutine parallel_combined
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc parallel loop gang reduction(+:sum) ! { dg-bogus "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do
end subroutine parallel_combined

subroutine serial_combined
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc serial loop gang reduction(+:sum) ! { dg-bogus "gang reduction on an orphan loop" }
  ! { dg-warning "region contains gang partitioned code but is not gang partitioned" "" { target *-*-* } .-1 }
  do i = 1, n
     sum = sum + 1
  end do
end subroutine serial_combined
