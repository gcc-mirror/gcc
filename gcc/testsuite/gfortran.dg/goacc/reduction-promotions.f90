! Ensure that each parallel reduction variable as a copy or pcopy
! data clause.

! { dg-additional-options "-fdump-tree-gimple" }

program test
  implicit none
  integer :: v1, v2

  !$acc parallel reduction(+:v1,v2)
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) copy(v1,v2)
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) pcopy(v1,v2)
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) present(v1,v2)
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) copyin(v1,v2) ! { dg-warning "incompatible data clause" }
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) pcopyin(v1,v2) ! { dg-warning "incompatible data clause" }
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) copyout(v1,v2) ! { dg-warning "incompatible data clause" }
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) pcopyout(v1,v2) ! { dg-warning "incompatible data clause" }
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) create(v1,v2) ! { dg-warning "incompatible data clause" }
  !$acc end parallel

  !$acc parallel reduction(+:v1,v2) pcreate(v1,v2) ! { dg-warning "incompatible data clause" }
  !$acc end parallel
end program test

! { dg-final { scan-tree-dump-times "map.tofrom:v1" 9 "gimple" } }
! { dg-final { scan-tree-dump-times "map.tofrom:v2" 9 "gimple" } }
! { dg-final { scan-tree-dump-times "map.force_present:v1" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "map.force_present:v2" 1 "gimple" } }
