! Test invalid calls to routines.

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

module param
  integer, parameter :: N = 32
end module param

program main
  use param
  integer :: i
  integer :: a(N)

  do i = 1, N
    a(i) = i
  end do

  !
  ! Seq routine tests.
  !

  !$acc parallel copy (a)
  !$acc loop
  do i = 1, N
     call seq (a)
  end do

  !$acc loop gang
  do i = 1, N
     call seq (a)
  end do

  !$acc loop worker
  do i = 1, N
     call seq (a)
  end do

  !$acc loop vector
  do i = 1, N
     call seq (a)
  end do
  !$acc end parallel

  !
  ! Gang routines loops.
  !

  !$acc parallel copy (a)
  !$acc loop ! { dg-warning "insufficient partitioning" }
  do i = 1, N
     call gang (a)
  end do

  !$acc loop gang ! { dg-message "containing loop" }
  do i = 1, N
     call gang (a) ! { dg-error "routine call uses same" }
  end do

  !$acc loop worker ! { dg-message "containing loop" }
  do i = 1, N
     call gang (a)  ! { dg-error "routine call uses same" }
  end do

  !$acc loop vector ! { dg-message "containing loop" }
  do i = 1, N
     call gang (a)   ! { dg-error "routine call uses same" }
  end do
  !$acc end parallel

  !
  ! Worker routines loops.
  !

  !$acc parallel copy (a)
  !$acc loop
  do i = 1, N
     call worker (a)
  end do

  !$acc loop gang
  do i = 1, N
     call worker (a)
  end do

  !$acc loop worker ! { dg-message "containing loop" }
  do i = 1, N
     call worker (a) ! { dg-error "routine call uses same" }
  end do

  !$acc loop vector ! { dg-message "containing loop" }
  do i = 1, N
     call worker (a) ! { dg-error "routine call uses same" }
  end do
  !$acc end parallel

  !
  ! Vector routines loops.
  !

  !$acc parallel copy (a)
  !$acc loop
  do i = 1, N
     call vector (a)
  end do

  !$acc loop gang
  do i = 1, N
     call vector (a)
  end do

  !$acc loop worker
  do i = 1, N
     call vector (a)
  end do

  !$acc loop vector ! { dg-message "containing loop" }
  do i = 1, N
     call vector (a) ! { dg-error "routine call uses same" }
  end do
  !$acc end parallel
contains

  subroutine gang (a) ! { dg-message "declared here" 3 }
    !$acc routine gang
    ! { dg-warning "region is gang partitioned but does not contain gang partitioned code" "" { target *-*-* } .-2 }
    ! { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-3 }
    ! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-4 }
    integer, intent (inout) :: a(N)
    integer :: i

    do i = 1, N
       a(i) = a(i) - a(i)
    end do
  end subroutine gang

  subroutine worker (a) ! { dg-message "declared here" 2 }
    !$acc routine worker
    ! { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-2 }
    ! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-3 }
    integer, intent (inout) :: a(N)
    integer :: i

    do i = 1, N
       a(i) = a(i) - a(i)
    end do
  end subroutine worker

  subroutine vector (a) ! { dg-message "declared here" }
    !$acc routine vector
    ! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-2 }
    integer, intent (inout) :: a(N)
    integer :: i

    do i = 1, N
       a(i) = a(i) - a(i)
    end do
  end subroutine vector

  subroutine seq (a)
    !$acc routine seq
    integer, intent (inout) :: a(N)
    integer :: i

    do i = 1, N
       a(i) = a(i) - a(i)
    end do
  end subroutine seq
end program main
