! { dg-do compile } 
! { dg-additional-options "-Wsurprising" }

module test
contains
  subroutine oacc1
    implicit none
    integer :: i
    integer, parameter :: a = 1
    !$acc declare device_resident (a) ! (no warning here - for semi-good reasons)
    !$acc data copy (a)  ! { dg-warning "Clause for object 'a' at .1. is ignored as parameters need not be copied \\\[-Wsurprising\\\]" }
    !$acc end data
    !$acc data deviceptr (a)  ! { dg-warning "Clause for object 'a' at .1. is ignored as parameters need not be copied \\\[-Wsurprising\\\]" }
    !$acc end data
    !$acc parallel private (a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    !$acc end parallel
    !$acc serial private (a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    !$acc end serial
    !$acc host_data use_device (a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    !$acc end host_data
    !$acc parallel loop reduction(+:a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    do i = 1,5
    enddo
    !$acc end parallel loop
    !$acc serial loop reduction(+:a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    do i = 1,5
    enddo
    !$acc end serial loop
    !$acc parallel loop
    do i = 1,5
      !$acc cache (a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    enddo
    !$acc end parallel loop
    !$acc serial loop
    do i = 1,5
      !$acc cache (a) ! { dg-warning "Clause for object 'a' at .1. is ignored as it is a parameter \\\[-Wsurprising\\\]" }
    enddo
    !$acc end serial loop
    !$acc update device (a)  ! { dg-warning "Clause for object 'a' at .1. is ignored as parameters need not be copied \\\[-Wsurprising\\\]" }
    !$acc update host (a)  ! { dg-warning "Clause for object 'a' at .1. is ignored as parameters need not be copied \\\[-Wsurprising\\\]" }
    !$acc update self (a)  ! { dg-warning "Clause for object 'a' at .1. is ignored as parameters need not be copied \\\[-Wsurprising\\\]" }
  end subroutine oacc1
end module test
