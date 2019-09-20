! { dg-do compile } 

module test
contains
  subroutine oacc1
    implicit none
    integer :: i
    integer, parameter :: a = 1
    !$acc declare device_resident (a) ! { dg-error "is not a variable" }
    !$acc data copy (a) ! { dg-error "not a variable" }
    !$acc end data
    !$acc data deviceptr (a) ! { dg-error "not a variable" }
    !$acc end data
    !$acc parallel private (a) ! { dg-error "not a variable" }
    !$acc end parallel
    !$acc host_data use_device (a) ! { dg-error "not a variable" }
    !$acc end host_data
    !$acc parallel loop reduction(+:a) ! { dg-error "not a variable" }
    do i = 1,5
    enddo
    !$acc end parallel loop
    !$acc parallel loop
    do i = 1,5
      !$acc cache (a) ! { dg-error "not a variable" }
    enddo
    !$acc end parallel loop
    !$acc update device (a) ! { dg-error "not a variable" }
    !$acc update host (a) ! { dg-error "not a variable" }
    !$acc update self (a) ! { dg-error "not a variable" }
  end subroutine oacc1
end module test
