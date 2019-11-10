! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

module test
contains
  subroutine assumed_size(a)
    implicit none
    integer :: a(*), i
    !$acc declare device_resident (a) ! { dg-error "Assumed size" }
    !$acc data copy (a) ! { dg-error "Assumed size" }
    !$acc end data
    !$acc data deviceptr (a) ! { dg-error "Assumed size" }
    !$acc end data
    !$acc parallel private (a) ! { dg-error "Assumed size" }
    !$acc end parallel
    !$acc serial private (a) ! { dg-error "Assumed size" }
    !$acc end serial
    !$acc host_data use_device (a) ! { dg-error "Assumed size" }
    !$acc end host_data
    !$acc parallel loop reduction(+:a) ! { dg-error "Assumed size" }
    ! { dg-error "Array 'a' is not permitted in reduction" "" { target "*-*-*" } .-1 }
    do i = 1,5
    enddo
    !$acc end parallel loop
    !$acc serial loop reduction(+:a) ! { dg-error "Assumed size" }
    ! { dg-error "Array 'a' is not permitted in reduction" "" { target "*-*-*" } .-1 }
    do i = 1,5
    enddo
    !$acc end serial loop
    !$acc update device (a) ! { dg-error "Assumed size" }
    !$acc update host (a) ! { dg-error "Assumed size" }
    !$acc update self (a) ! { dg-error "Assumed size" }
  end subroutine assumed_size
  subroutine assumed_rank(a)
    implicit none
    integer, intent(in) :: a(..)
    integer :: i
    !$acc declare device_resident (a) ! { dg-error "Assumed rank" }
    !$acc data copy (a) ! { dg-error "Assumed rank" }
    !$acc end data
    !$acc data deviceptr (a) ! { dg-error "Assumed rank" }
    !$acc end data
    !$acc parallel private (a) ! { dg-error "Assumed rank" }
    !$acc end parallel
    !$acc serial private (a) ! { dg-error "Assumed rank" }
    !$acc end serial
    !$acc host_data use_device (a) ! { dg-error "Assumed rank" }
    !$acc end host_data
    !$acc parallel loop reduction(+:a) ! { dg-error "Assumed rank" }
    ! { dg-error "Array 'a' is not permitted in reduction" "" { target "*-*-*" } .-1 }
    do i = 1,5
    enddo
    !$acc end parallel loop
    !$acc serial loop reduction(+:a) ! { dg-error "Assumed rank" }
    ! { dg-error "Array 'a' is not permitted in reduction" "" { target "*-*-*" } .-1 }
    do i = 1,5
    enddo
    !$acc end serial loop
    !$acc update device (a) ! { dg-error "Assumed rank" }
    !$acc update host (a) ! { dg-error "Assumed rank" }
    !$acc update self (a) ! { dg-error "Assumed rank" }
  end subroutine assumed_rank
end module test
