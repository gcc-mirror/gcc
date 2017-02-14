! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original" } 

program test
  implicit none
  integer, pointer :: p

  !$acc host_data use_device(p)
  !$acc end host_data
end program test
! { dg-final { scan-tree-dump-times "pragma acc host_data use_device_ptr\\(p\\)" 1 "original" } }
