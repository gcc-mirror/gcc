! { dg-additional-options "-fdump-tree-original" }

! test for tree-dump-original and spaces-commas

program test
  implicit none
  integer :: q, i, j, k, m, n, o, p, r, s, t, u, v, w
  logical :: l = .true.

  !$acc parallel if(l) async num_gangs(i) num_workers(i) vector_length(i) &
  !$acc reduction(max:q), copy(i), copyin(j), copyout(k), create(m) &
  !$acc present(o), pcopy(p), pcopyin(r), pcopyout(s), pcreate(t) &
  !$acc deviceptr(u), private(v), firstprivate(w)
  !$acc end parallel

end program test

! { dg-final { scan-tree-dump-times "pragma acc parallel" 1 "original" } } 

! { dg-final { scan-tree-dump-times "if" 1 "original" } }
! { dg-final { scan-tree-dump-times "async" 1 "original" } } 
! { dg-final { scan-tree-dump-times "num_gangs" 1 "original" } } 
! { dg-final { scan-tree-dump-times "num_workers" 1 "original" } } 
! { dg-final { scan-tree-dump-times "vector_length" 1 "original" } } 

! { dg-final { scan-tree-dump-times "reduction\\(max:q\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(tofrom:i\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(to:j\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(from:k\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(alloc:m\\)" 1 "original" } } 

! { dg-final { scan-tree-dump-times "map\\(force_present:o\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(tofrom:p\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(to:r\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(from:s\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "map\\(alloc:t\\)" 1 "original" } } 

! { dg-final { scan-tree-dump-times "map\\(force_deviceptr:u\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "private\\(v\\)" 1 "original" } } 
