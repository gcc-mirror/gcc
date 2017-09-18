! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/70071
! Based on testcases by Gerhard Steinmetz

program pr70071
  implicit none
  integer, allocatable :: z(:)[:,:]
  allocate (z(2)[1::2,*])  ! { dg-error "Bad array dimension" }
  allocate (z(1::2)[2,*])  ! { dg-error "Bad array specification in ALLOCATE" }
end program pr70071
