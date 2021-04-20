! { dg-do compile } 
! { dg-additional-options "-fdump-tree-original" } 
! { dg-additional-options "--param=openacc-kernels=decompose" }
! { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose" }

program test
  implicit none
  integer :: q, i, j, k, m, n, o, p, r, s, t, u, v, w
  logical :: l = .true.

  !$acc kernels if(l) async num_gangs(i) num_workers(i) vector_length(i) &
  !$acc copy(i), copyin(j), copyout(k), create(m) &
  !$acc no_create(n) &
  !$acc present(o), pcopy(p), pcopyin(r), pcopyout(s), pcreate(t) &
  !$acc deviceptr(u)
  !$acc end kernels

end program test
! { dg-final { scan-tree-dump-times "pragma acc kernels" 1 "original" } } 

! { dg-final { scan-tree-dump-times "if" 1 "original" } }
! { dg-final { scan-tree-dump-times "async" 1 "original" } } 
! { dg-final { scan-tree-dump-times "num_gangs" 1 "original" } } 
! { dg-final { scan-tree-dump-times "num_workers" 1 "original" } } 
! { dg-final { scan-tree-dump-times "vector_length" 1 "original" } } 

! { dg-final { scan-tree-dump-times "map\\(tofrom:i\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(to:j\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(from:k\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(alloc:m\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(no_alloc:n\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(force_present:o\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(tofrom:p\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(to:r\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(from:s\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(alloc:t\\)" 1 "original" } } 

! { dg-final { scan-tree-dump-times "map\\(force_deviceptr:u\\)" 1 "original" } } 

! { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_data_kernels if\(D\.[0-9]+\)$} 1 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel_kernels_gang_single num_gangs\(1\) if\(D\.[0-9]+\) async\(-1\)$} 1 "omp_oacc_kernels_decompose" } }
