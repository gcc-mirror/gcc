! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
! Tests whether volatile really works for arrays
! PR fortran/29601
logical, allocatable, volatile :: t1(:)
logical, allocatable :: t2(:)
integer :: i

allocate(t1(1),t2(1))
t1 = .false.
t2 = .false.
do i = 1, 2
  if(ubound(t1,1) /= 1) print *, 'VolatileNotOptimizedAway1'
  if(ubound(t2,1) /= 1) print *, 'NonVolatileNotOptimizedAway1'
end do

t1 = .false.
if(t1(1)) print *, 'VolatileNotOptimizedAway2'
t2 = .false.
if(t2(1)) print *, 'NonVolatileNotOptimizedAway2'
end
! { dg-final { scan-tree-dump "VolatileNotOptimizedAway1" "optimized" } }
! { dg-final { scan-tree-dump "VolatileNotOptimizedAway2" "optimized" } }
! { dg-final { scan-tree-dump-not "NonVolatileNotOptimizedAway1" "optimized" } }
! { dg-final { scan-tree-dump-not "NonVolatileNotOptimizedAway2" "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
