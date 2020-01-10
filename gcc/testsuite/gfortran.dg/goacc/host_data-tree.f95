! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original" } 

program test
  implicit none
  integer, pointer :: p

  !$acc host_data use_device(p)
  !$acc end host_data

  !$acc host_data use_device(p) if (p == 42)
  !$acc end host_data

  !$acc host_data use_device(p) if_present if (p == 43)
  !$acc end host_data
end program test
! { dg-final { scan-tree-dump-times "pragma acc host_data use_device_ptr\\(p\\)" 3 "original" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = \\*p == 42;" 1 "original" } }
! { dg-final { scan-tree-dump-times "pragma acc host_data use_device_ptr\\(p\\) if\\(D.\[0-9\]+\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = \\*p == 43;" 1 "original" } }
! { dg-final { scan-tree-dump-times "pragma acc host_data use_device_ptr\\(p\\) if\\(D.\[0-9\]+\\) if_present" 1 "original" } }
