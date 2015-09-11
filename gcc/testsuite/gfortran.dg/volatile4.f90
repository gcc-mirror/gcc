! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
! Tests whether volatile really works
! PR fortran/29601
logical, volatile :: t1
logical :: t2
integer :: i

t2 = .false.
t1 = .false.
do i = 1, 2
  if(t1) print *, 'VolatileNotOptimizedAway'
  if(t2) print *, 'NonVolatileNotOptimizedAway'
end do
end
! { dg-final { scan-tree-dump "VolatileNotOptimizedAway" "optimized" } } */
! { dg-final { scan-tree-dump-not "NonVolatileNotOptimizedAway" "optimized" } } */
