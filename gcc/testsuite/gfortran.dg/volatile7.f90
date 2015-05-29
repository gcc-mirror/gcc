! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
! Tests whether volatile really works for pointers
! PR fortran/29601
logical, pointer, volatile :: t1
logical, pointer :: t2
integer :: i

t1 => NULL(t1)
if(associated(t1)) print *, 'VolatileNotOptimizedAway'
t2 => NULL(t2)
if(associated(t2)) print *, 'NonVolatileNotOptimizedAway'
end
! { dg-final { scan-tree-dump "VolatileNotOptimizedAway" "optimized" } }
! { dg-final { scan-tree-dump-not "NonVolatileNotOptimizedAway" "optimized" } } 
