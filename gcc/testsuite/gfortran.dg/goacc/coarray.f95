! { dg-do compile } 
! { dg-additional-options "-fcoarray=single" }
!
! PR fortran/63861

module test
contains
  subroutine oacc1(a)
    implicit none
    integer :: i
    integer, codimension[*] :: a
    !$acc declare device_resident (a)
    !$acc data copy (a)
    !$acc end data
    !$acc data deviceptr (a)
    !$acc end data
    !$acc parallel private (a)
    !$acc end parallel
    !$acc host_data use_device (a)
    !$acc end host_data
    !$acc parallel loop reduction(+:a) ! { dg-error "Array 'a' is not permitted in reduction" }
    do i = 1,5
    enddo
    !$acc end parallel loop
    !$acc parallel loop
    do i = 1,5
      !$acc cache (a) ! { dg-error "" "TODO" { xfail *-*-* } }
    enddo
    !$acc end parallel loop
    !$acc update device (a)
    !$acc update host (a)
    !$acc update self (a)
  end subroutine oacc1
end module test
