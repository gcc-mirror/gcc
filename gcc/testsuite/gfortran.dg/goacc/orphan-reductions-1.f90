! Verify that gang reduction on orphan OpenACC loops reported as errors.

! { dg-do compile }

subroutine s1
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc parallel reduction(+:sum)
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end parallel

  !$acc parallel loop gang reduction(+:sum)
  do i = 1, n
     sum = sum + 1
  end do

  !$acc parallel
  !$acc loop gang reduction(+:sum)
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end parallel
end subroutine s1

subroutine s2
  implicit none
  !$acc routine gang

  integer, parameter :: n = 100
  integer :: i, j, sum
  sum = 0

  !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do

  !$acc loop reduction(+:sum)
  ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
  do i = 1, n
     !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
     do j = 1, n
        sum = sum + 1
     end do
  end do
end subroutine s2

integer function f1 ()
  implicit none

  integer, parameter :: n = 100
  integer :: i, sum
  sum = 0

  !$acc parallel reduction(+:sum)
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end parallel

  !$acc parallel loop gang reduction(+:sum)
  do i = 1, n
     sum = sum + 1
  end do

  !$acc parallel
  !$acc loop gang reduction(+:sum)
  do i = 1, n
     sum = sum + 1
  end do
  !$acc end parallel

  f1 = sum
end function f1

integer function f2 ()
  implicit none
  !$acc routine gang

  integer, parameter :: n = 100
  integer :: i, j, sum
  sum = 0

  !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
  do i = 1, n
     sum = sum + 1
  end do

  !$acc loop reduction(+:sum)
  ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
  do i = 1, n
     !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
     do j = 1, n
        sum = sum + 1
     end do
  end do

  f2 = sum
end function f2

module m
contains
  subroutine s3
    implicit none

    integer, parameter :: n = 100
    integer :: i, sum
    sum = 0

    !$acc parallel reduction(+:sum)
    do i = 1, n
       sum = sum + 1
    end do
    !$acc end parallel

    !$acc parallel loop gang reduction(+:sum)
    do i = 1, n
       sum = sum + 1
    end do

    !$acc parallel
    !$acc loop gang reduction(+:sum)
    do i = 1, n
       sum = sum + 1
    end do
    !$acc end parallel
  end subroutine s3

  subroutine s4
    implicit none
    !$acc routine gang

    integer, parameter :: n = 100
    integer :: i, j, sum
    sum = 0

    !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
    do i = 1, n
       sum = sum + 1
    end do

    !$acc loop reduction(+:sum)
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, n
       !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
       do j = 1, n
          sum = sum + 1
       end do
    end do
  end subroutine s4

  integer function f3 ()
    implicit none

    integer, parameter :: n = 100
    integer :: i, sum
    sum = 0

    !$acc parallel reduction(+:sum)
    do i = 1, n
       sum = sum + 1
    end do
    !$acc end parallel

    !$acc parallel loop gang reduction(+:sum)
    do i = 1, n
       sum = sum + 1
    end do

    !$acc parallel
    !$acc loop gang reduction(+:sum)
    do i = 1, n
       sum = sum + 1
    end do
    !$acc end parallel

    f3 = sum
  end function f3

  integer function f4 ()
    implicit none
    !$acc routine gang

    integer, parameter :: n = 100
    integer :: i, j, sum
    sum = 0

    !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
    do i = 1, n
       sum = sum + 1
    end do

    !$acc loop reduction(+:sum)
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, n
       !$acc loop gang reduction(+:sum) ! { dg-error "gang reduction on an orphan loop" }
       do j = 1, n
          sum = sum + 1
       end do
    end do

    f4 = sum
  end function f4
end module m
