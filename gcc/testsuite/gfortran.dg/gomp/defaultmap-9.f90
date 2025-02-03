! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

subroutine f
  implicit none
  type t
    integer :: i
  end type t
  integer, target :: scalar
  integer, target :: array(5)
  integer, pointer :: ptr1, ptr2(:)
  integer, allocatable :: alloc1, alloc2(:)
  type(t) :: agg1, agg2(2)

  scalar = 1
  array = [1,2,3,4,5]
  ptr1 => scalar
  ptr2 => array
  alloc1 = 5
  alloc2 = [1,2]
  agg1%i = 1
  agg2(:)%i = [1,2]

  ! firstprivate + unspecified modifer.
  !$omp target defaultmap(firstprivate)
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  ! equivalent: firstprivate + ALL modifer.
  !$omp target defaultmap(firstprivate : all)
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  ! tofrom + ALL modifer.
  !$omp target defaultmap(tofrom : all)
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block
end subroutine

! { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(firstprivate\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(firstprivate:all\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(tofrom:all\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp target.* defaultmap\\(firstprivate\\) firstprivate\\(scalar\\) firstprivate\\(ptr2\\) firstprivate\\(ptr1\\) firstprivate\\(array\\) firstprivate\\(alloc2\\) firstprivate\\(alloc1\\) firstprivate\\(agg2\\) firstprivate\\(agg1\\)" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "#pragma omp target.* defaultmap\\(firstprivate:all\\) firstprivate\\(scalar\\) firstprivate\\(ptr2\\) firstprivate\\(ptr1\\) firstprivate\\(array\\) firstprivate\\(alloc2\\) firstprivate\\(alloc1\\) firstprivate\\(agg2\\) firstprivate\\(agg1\\)" 1 "gimple" } }

! { dg-final { scan-tree-dump-times "#pragma omp target.* defaultmap\\(tofrom:all\\) map\\(tofrom:scalar \\\[len: .\\\] \\\[runtime_implicit\\\]\\) map\\(tofrom:.*ptr2.data \\\[len: .*\\\] \\\[runtime_implicit\\\]\\) map\\(to:ptr2 \\\[pointer set, len: ..\\\]\\) map\\(always_pointer:.*ptr2.data \\\[pointer assign, bias: 0\\\]\\) map\\(tofrom:\\*ptr1 \\\[len: .\\\] \\\[runtime_implicit\\\]\\) map\\(alloc:ptr1 \\\[pointer assign, bias: 0\\\]\\) map\\(tofrom:array \\\[len: ..\\\] \\\[runtime_implicit\\\]\\) map\\(tofrom:.*alloc2.data \\\[len: .*\\\] \\\[runtime_implicit\\\]\\) map\\(to:alloc2 \\\[pointer set, len: ..\\\]\\) map\\(alloc:.*alloc2.data \\\[pointer assign, bias: 0\\\]\\) map\\(tofrom:\\*alloc1 \\\[len: .\\\] \\\[runtime_implicit\\\]\\) map\\(alloc:alloc1 \\\[pointer assign, bias: 0\\\]\\) map\\(tofrom:agg2 \\\[len: .\\\] \\\[runtime_implicit\\\]\\) map\\(tofrom:agg1 \\\[len: .\\\] \\\[runtime_implicit\\\]\\)" 1 "gimple" } }
