! { dg-do compile } 

module test
contains
  subroutine oacc1
    implicit none
    integer :: i
    !$acc declare device_resident (10) ! { dg-error "Syntax error" }
    !$acc data copy (10) ! { dg-error "Syntax error" }
    !$acc end data ! { dg-error "Unexpected" }
    !$acc data deviceptr (10) ! { dg-error "Syntax error" }
    !$acc end data ! { dg-error "Unexpected" }
    !$acc data private (10) ! { dg-error "Failed to match clause" }
    !$acc end data ! { dg-error "Unexpected" }
    !$acc host_data use_device (10) ! { dg-error "Syntax error" }
    !$acc end host_data ! { dg-error "Unexpected" }
    !$acc parallel loop reduction(+:10) ! { dg-error "Syntax error" }
    do i = 1,5
    enddo
    !$acc end parallel loop ! { dg-error "Unexpected" }
    !$acc serial loop reduction(+:10) ! { dg-error "Syntax error" }
    do i = 1,5
    enddo
    !$acc end serial loop ! { dg-error "Unexpected" }
    !$acc parallel loop
    do i = 1,5
      !$acc cache (10) ! { dg-error "Syntax error" }
    enddo
    !$acc end parallel loop
    !$acc serial loop
    do i = 1,5
      !$acc cache (10) ! { dg-error "Syntax error" }
    enddo
    !$acc end serial loop
    !$acc update device (10) ! { dg-error "Syntax error" }
    !$acc update host (10) ! { dg-error "Syntax error" }
    !$acc update self (10) ! { dg-error "Syntax error" }
  end subroutine oacc1
end module test
