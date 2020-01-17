! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

program test
  implicit none
  integer, pointer :: p

  !$acc host_data use_device(p)
  ! { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data use_device_ptr\\(p\\)$" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data use_device_ptr\\(p\\)$" 1 "gimple" } }
  !$acc end host_data

  !$acc host_data use_device(p) if (p == 42)
  ! { dg-final { scan-tree-dump-times "(?n)D\\.\[0-9\]+ = \\*p == 42;$" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data use_device_ptr\\(p\\) if\\(D\\.\[0-9\]+\\)$" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data use_device_ptr\\(p\\) if\\(D\\.\[0-9\]+\\)$" 1 "gimple" } }
  !$acc end host_data

  !$acc host_data use_device(p) if_present if (p == 43)
  ! { dg-final { scan-tree-dump-times "(?n)D\\.\[0-9\]+ = \\*p == 43;$" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data use_device_ptr\\(p\\) if\\(D\\.\[0-9\]+\\) if_present$" 1 "original" } }
  ! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data use_device_ptr\\(if_present:p\\) if\\(D\\.\[0-9\]+\\) if_present$" 1 "gimple" } }
  !$acc end host_data
end program test
