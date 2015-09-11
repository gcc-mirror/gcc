! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
! Tests that volatile can be applied to members of common blocks or
! equivalence groups (PR fortran/35037)
!
subroutine wait1
  logical event
  volatile event
  common /dd/ event
  event = .false.
  do
    if (event) print *, 'NotOptimizedAway1'
  end do
end subroutine

subroutine wait2
  logical event, foo
  volatile event
  equivalence (event, foo)
  event = .false.
  do
    if (event) print *, 'NotOptimizedAway2'
  end do
end subroutine

subroutine wait3
  logical event
  integer foo
  volatile foo
  equivalence (event, foo)
  event = .false.
  do
    if (event) print *, 'IsOptimizedAway'
  end do
end subroutine

! { dg-final { scan-tree-dump "NotOptimizedAway1" "optimized" } } */
! { dg-final { scan-tree-dump "NotOptimizedAway2" "optimized" } } */
! { dg-final { scan-tree-dump-not "IsOptimizedAway" "optimized" } } */
