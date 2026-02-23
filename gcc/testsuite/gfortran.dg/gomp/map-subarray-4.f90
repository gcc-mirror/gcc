! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! PR fortran/120505

! Check that struct components are mapped in increasing address order.

module m
type t
 integer, allocatable :: den1(:,:), den2(:,:)
end type t

type t2
 type(t), allocatable :: tiles(:)
end type t2

type(t2) :: var
end

use m

allocate(var%tiles(1))
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])

!$omp target enter data map(var%tiles(1)%den2, var%tiles(1)%den1)

! { dg-final { scan-tree-dump { map\(struct_unord:MEM <struct t\[0:\]> \[\(struct t\[0:\] \*\)_[0-9]+\] \[len: 2\]\) map\(to:MEM <struct t\[0:\]> \[\(struct t\[0:\] \*\)_[0-9]+\]\[_[0-9]+\]\.den[12] \[pointer set, len: (?:48|88)]\) map\(to:MEM <struct t\[0:\]> \[\(struct t\[0:\] \*\)_[0-9]+\]\[_[0-9]+\]\.den[12] \[pointer set, len: (?:48|88)\]\) } "gimple" } }

!$omp target exit data map(var%tiles(1)%den2, var%tiles(1)%den1)

! { dg-final { scan-tree-dump { map\(release:MEM <struct t\[0:\]> \[\(struct t\[0:\] \*\)_[0-9]+\]\[_[0-9]+\]\.den[12] \[pointer set, len: (?:48|88)\]\) map\(release:MEM <struct t\[0:\]> \[\(struct t\[0:\] \*\)_[0-9]+\]\[_[0-9]+\]\.den[12] \[pointer set, len: (?:48|88)\]\) } "gimple" } }

end
