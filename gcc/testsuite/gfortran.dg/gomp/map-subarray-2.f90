! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

type T
integer, pointer :: arr1(:)
integer, pointer :: arr2(:)
integer, pointer :: arr3(:)
integer, pointer :: arr4(:)
end type T

type(T) :: tv
integer, allocatable, target, dimension(:) :: arr

allocate(arr(1:20))

tv%arr1 => arr
tv%arr2 => arr
tv%arr3 => arr
tv%arr4 => arr

!$omp target enter data map(to: tv%arr1)

! { dg-final { scan-tree-dump {(?n)#pragma omp target enter data map\(struct:tv \[len: 1\]\) map\(to:tv\.arr1 \[pointer set, len: [0-9]+\]\) map\(to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:tv\.arr1\.data \[bias: 0\]\)} "gimple" } }

!$omp target exit data map(from: tv%arr1)

! { dg-final { scan-tree-dump {(?n)#pragma omp target exit data map\(release:tv\.arr1 \[pointer set, len: [0-9]+\]\) map\(from:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(detach:tv\.arr1\.data \[bias: 0\]\)} "gimple" } }


!$omp target enter data map(to: tv%arr2) map(to: tv%arr2(1:10))

! { dg-final { scan-tree-dump {(?n)#pragma omp target enter data map\(struct:tv \[len: 1\]\) map\(to:tv\.arr2 \[pointer set, len: [0-9]+\]\) map\(to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:tv\.arr2\.data \[bias: [^\]]+\]\)} "gimple" } }

!$omp target exit data map(from: tv%arr2) map(from: tv%arr2(1:10))

! { dg-final { scan-tree-dump {(?n)#pragma omp target exit data map\(release:tv\.arr2 \[pointer set, len: [0-9]+\]\) map\(from:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(detach:tv\.arr2\.data \[bias: [^\]]+\]\)} "gimple" } }


!$omp target enter data map(to: tv, tv%arr3(1:10))

! { dg-final { scan-tree-dump {(?n)#pragma omp target enter data map\(to:tv \[len: [0-9]+\]\) map\(to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:tv\.arr3\.data \[bias: [^\]]+\]\)} "gimple" } }

!$omp target exit data map(from: tv, tv%arr3(1:10))

! { dg-final { scan-tree-dump {(?n)#pragma omp target exit data map\(from:tv \[len: [0-9]+\]\) map\(from:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)[_[0-9]+\] \[len: _[0-9]+\]\) map\(detach:tv\.arr3\.data \[bias: [^\]]+\]\)} "gimple" } }


!$omp target enter data map(to: tv%arr4(1:10))

! { dg-final { scan-tree-dump {(?n)#pragma omp target enter data map\(struct:tv \[len: 1\]\) map\(to:tv\.arr4 \[pointer set, len: [0-9]+\]\) map\(to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:tv\.arr4\.data \[bias: [^\]]+\]\)} "gimple" } }

!$omp target exit data map(from: tv%arr4(1:10))

! { dg-final { scan-tree-dump {(?n)#pragma omp target exit data map\(release:tv\.arr4 \[pointer set, len: [0-9]+\]\) map\(from:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(detach:tv\.arr4\.data \[bias: [^\]]+\]\)} "gimple" } }

end

