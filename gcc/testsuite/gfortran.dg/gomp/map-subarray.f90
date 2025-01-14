! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

type T
integer, pointer :: arr1(:)
integer, pointer :: arr2(:)
end type T

type(T) :: tv
integer, allocatable, target, dimension(:) :: arr

allocate(arr(1:20))

tv%arr1 => arr
tv%arr2 => arr

!$omp target map(tv%arr1)
tv%arr1(1) = tv%arr1(1) + 1
!$omp end target

! { dg-final { scan-tree-dump {(?n)#pragma omp target.* map\(struct:tv \[len: 1\]\) map\(to:tv\.arr1 \[pointer set, len: [0-9]+\]\) map\(tofrom:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\] \[runtime_implicit\]\) map\(attach:tv\.arr1\.data \[bias: 0\]\)} "gimple" } }

!$omp target map(tv%arr2) map(tv%arr2(1:10))
tv%arr2(1) = tv%arr2(1) + 1
!$omp end target

!$omp target map(tv%arr2(1:10))
tv%arr2(1) = tv%arr2(1) + 1
!$omp end target

! { dg-final { scan-tree-dump-times {(?n)#pragma omp target.* map\(struct:tv \[len: 1\]\) map\(to:tv\.arr2 \[pointer set, len: [0-9]+\]\) map\(tofrom:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:tv\.arr2\.data \[bias: [^\]]+\]\)} 2 "gimple" } }

!$omp target map(tv, tv%arr2(1:10))
tv%arr2(1) = tv%arr2(1) + 1
!$omp end target

! { dg-final { scan-tree-dump {(?n)#pragma omp target.* map\(tofrom:tv \[len: [0-9]+\]\) map\(tofrom:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:tv\.arr2\.data \[bias: [^\]]+\]\)} "gimple" } }

end

