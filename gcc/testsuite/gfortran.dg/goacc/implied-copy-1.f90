! { dg-additional-options "-fdump-tree-gimple" }

! Test for implied copy of reduction variable on combined construct.

subroutine test
  implicit none
  integer a(100), i, s, p
  p = 1

  !$acc parallel loop reduction(+:s) reduction(*:p)
  do i = 1, 100
    s = s + a(i)
    p = p * a(i)
  end do
  !$acc end parallel loop

  !$acc serial loop reduction(+:s) reduction(*:p)
  do i = 1, 100
    s = s + a(i)
    p = p * a(i)
  end do
  !$acc end serial loop

  !$acc kernels loop reduction(+:s) reduction(*:p)
  do i = 1, 100
    s = s + a(i)
    p = p * a(i)
  end do
  !$acc end kernels loop
end subroutine test

! { dg-final { scan-tree-dump-times "map\\(force_tofrom:s \\\[len: \[0-9\]+\\\]\\)" 1 "gimple" } } 
! { dg-final { scan-tree-dump-times "map\\(force_tofrom:p \\\[len: \[0-9\]+\\\]\\)" 1 "gimple" } } 
! { dg-final { scan-tree-dump-times "map\\(tofrom:s \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } 
! { dg-final { scan-tree-dump-times "map\\(tofrom:p \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } 
