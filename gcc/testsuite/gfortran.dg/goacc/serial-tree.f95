! { dg-additional-options "-fdump-tree-original" }

! test for tree-dump-original and spaces-commas

! { dg-additional-options -Wuninitialized }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

program test
  implicit none
  integer :: q, i, j, k, m, n, o, p, r, s, t, u, v, w
  ! { dg-note {'i' was declared here} {} { xfail *-*-* } .-1 }
  logical :: l = .true.

  !$acc serial if(l) async &
  !$acc self &
  !$acc reduction(max:q), copy(i), copyin(j), copyout(k), create(m) &
  !$acc no_create(n) &
  !$acc present(o), pcopy(p), pcopyin(r), pcopyout(s), pcreate(t) &
  !$acc deviceptr(u), private(v), firstprivate(w)
  ! { dg-warning {'i' is used uninitialized} {} { xfail *-*-* } .-1 }
  !$acc end serial

end program test

! { dg-final { scan-tree-dump-times "pragma acc serial" 1 "original" } } 

! { dg-final { scan-tree-dump-times "if" 1 "original" } }
! { dg-final { scan-tree-dump-times "async" 1 "original" } } 
! { dg-final { scan-tree-dump-times "self\\(1\\)" 1 "original" } } 
! { dg-final { scan-tree-dump-times "reduction\\(max:q\\)" 1 "original" } } 
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
! { dg-final { scan-tree-dump-times "private\\(v\\)" 1 "original" } } 
