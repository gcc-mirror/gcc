! { dg-additional-options "-fdump-tree-original" }
!
! In OpenMP 5.2 permits tofrom for enter/exit data
! in the FE, it is already converted to 'to' and 'from', respectively.
module m
  integer :: x, y, z
contains
subroutine copyin
  !$omp target enter data map(x) map(tofrom: y) map(always, tofrom: z)
end
subroutine copyout
  !$omp target exit data map(x) map(tofrom: y) map(always, tofrom: z)
end
end

! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:x\\) map\\(to:y\\) map\\(always,to:z\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(from:x\\) map\\(from:y\\) map\\(always,from:z\\)" 1 "original" } }
