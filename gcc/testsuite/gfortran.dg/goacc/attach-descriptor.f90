! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

program att
  implicit none
  type t
    integer :: arr1(10)
    integer, allocatable :: arr2(:)
  end type t
  type(t) :: myvar
  integer, target :: tarr(10)
  integer, pointer :: myptr(:)

  !$acc enter data attach(myvar%arr2, myptr)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc enter data map\\(attach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(attach:\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) myptr\\.data \\\[bias: 0\\\]\\);$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_data map\\(attach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(attach:myptr\\.data \\\[bias: 0\\\]\\)$" 1 "gimple" } }

  !$acc exit data detach(myvar%arr2, myptr)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(detach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(detach:\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) myptr\\.data \\\[bias: 0\\\]\\);$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(detach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(detach:myptr\\.data \\\[bias: 0\\\]\\)$" 1 "gimple" } }

  ! Test valid usage and processing of the finalize clause.
  !$acc exit data detach(myvar%arr2, myptr) finalize
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(detach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(detach:\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) myptr\\.data \\\[bias: 0\\\]\\) finalize;$" 1 "original" } }
  ! For array-descriptor detaches, we no longer generate a "release" mapping
  ! for the pointed-to data for gimplify.c to turn into "delete".  Make sure
  ! the mapping still isn't there.
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(force_detach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(force_detach:myptr\\.data \\\[bias: 0\\\]\\) finalize$" 1 "gimple" } }

end program att
