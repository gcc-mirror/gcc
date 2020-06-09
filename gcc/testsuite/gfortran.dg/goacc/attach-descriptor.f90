! { dg-additional-options "-fdump-tree-original" }

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
! { dg-final { scan-tree-dump-times "(?n)#pragma acc enter data map\\(attach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(alloc:\\*\\(c_char \\*\\) myptr\\.data \\\[len: \[^\\\]\]+\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(attach:\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) myptr\\.data \\\[bias: 0\\\]\\);$" 1 "original" } }

  !$acc exit data detach(myvar%arr2, myptr)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(detach:myvar\\.arr2 \\\[bias: 0\\\]\\) map\\(release:\\*\\(c_char \\*\\) myptr\\.data \\\[len: \[^\\\]\]+\\\]\\) map\\(to:myptr \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(detach:\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) myptr\\.data \\\[bias: 0\\\]\\);$" 1 "original" } }
end program att
