! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original" } 

program test
  implicit none
  integer :: i = 1

  !$acc host_data use_device(i)
  !$acc end host_data
end program test
! { dg-final { scan-tree-dump-times "pragma acc host_data use_device_ptr\\(i\\)" 1 "original" } }
