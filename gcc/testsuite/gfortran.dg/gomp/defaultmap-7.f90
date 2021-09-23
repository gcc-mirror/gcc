! PR fortran/92568
!
! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-gimple" }
implicit none
  integer :: ii, aa, pp
  allocatable :: aa
  pointer :: pp
  character :: str
  character(len=2) :: str2

!$omp target
  ii = 1
  aa = 5
  pp = 7
  str = '1'
  str2 = '12'
!$omp end target
end
! { dg-final { scan-tree-dump-times "firstprivate\\(ii\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*aa" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:\\*pp" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:str2 \\\[len:" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:str \\\[len:" 1 "gimple" } }
