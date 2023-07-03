! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

type t
integer, allocatable :: arrcomp(:)
integer :: b, c, d
end type t

type(t) :: myvar

!$omp declare mapper (t :: x) map(to: x%arrcomp) map(alloc: x%b) &
!$omp &                       map(from: x%c) map(tofrom: x%d)

allocate (myvar%arrcomp(1:100))

!$omp target enter data map(to: myvar)

! { dg-final { scan-tree-dump-times {map\(struct:myvar \[len: 4\]\) map\(to:myvar\.arrcomp \[pointer set, len: [0-9]+\]\) map\(alloc:myvar\.b \[len: [0-9]+\]\) map\(alloc:myvar\.c \[len: 4\]\) map\(to:myvar\.d \[len: [0-9]+\]\) map\(to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:myvar\.arrcomp\.data \[bias: 0\]\)} 1 "gimple" } }

!$omp target exit data map(from: myvar)

! { dg-final { scan-tree-dump-times {map\(release:myvar\.b \[len: [0-9]+\]\) map\(from:myvar\.c \[len: [0-9]+\]\) map\(from:myvar\.d \[len: [0-9]+\]\) map\(release:myvar\.arrcomp \[len: [0-9]+\]\) map\(detach:myvar\.arrcomp\.data \[bias: 0\]\) map\(release:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\)} 1 "gimple" } }


!$omp target enter data map(alloc: myvar)

! { dg-final { scan-tree-dump-times {map\(struct:myvar \[len: 4\]\) map\(to:myvar\.arrcomp \[pointer set, len: [0-9]+\]\) map\(alloc:myvar\.b \[len: [0-9]+\]\) map\(alloc:myvar\.c \[len: [0-9]+\]\) map\(alloc:myvar\.d \[len: [0-9]+\]\) map\(alloc:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:myvar\.arrcomp\.data \[bias: 0\]\)} 1 "gimple" } }

!$omp target exit data map(release: myvar)

! { dg-final { scan-tree-dump-times {map\(release:myvar\.b \[len: [0-9]+\]\) map\(release:myvar\.c \[len: [0-9]+\]\) map\(release:myvar\.d \[len: [0-9]+\]\) map\(release:myvar\.arrcomp \[len: [0-9]+\]\) map\(detach:myvar\.arrcomp\.data \[bias: 0\]\) map\(release:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\)} 1 "gimple" } }


!$omp target enter data map(present, to: myvar)

! { dg-final { scan-tree-dump-times {map\(force_present:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:myvar\.arrcomp\.data \[bias: 0\]\) map\(struct:myvar \[len: 4\]\) map\(to:myvar\.arrcomp \[pointer set, len: [0-9]+\]\) map\(force_present:myvar\.b \[len: [0-9]+\]\) map\(force_present:myvar\.c \[len: [0-9]+\]\) map\(force_present:myvar\.d \[len: [0-9]+\]\)} 1 "gimple" } }

!$omp target exit data map(present, from: myvar)

! { dg-final { scan-tree-dump-times {map\(release:myvar\.b \[len: [0-9]+\]\) map\(force_present:myvar\.c \[len: [0-9]+\]\) map\(force_present:myvar\.d \[len: [0-9]+\]\) map\(release:myvar\.arrcomp \[len: [0-9]+\]\) map\(detach:myvar\.arrcomp\.data \[bias: 0\]\) map\(release:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\)} 1 "gimple" } }


!$omp target enter data map(always, to: myvar)

! { dg-final { scan-tree-dump-times {map\(struct:myvar \[len: 4\]\) map\(to:myvar\.arrcomp \[pointer set, len: [0-9]+\]\) map\(alloc:myvar\.b \[len: [0-9]+\]\) map\(alloc:myvar\.c \[len: [0-9]+\]\) map\(always,to:myvar\.d \[len: [0-9]+\]\) map\(always,to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:myvar\.arrcomp\.data \[bias: 0\]\)} 1 "gimple" } }

!$omp target exit data map(always, from: myvar)

! { dg-final { scan-tree-dump-times {map\(release:myvar\.b \[len: [0-9]+\]\) map\(always,from:myvar\.c \[len: [0-9]+\]\) map\(always,from:myvar\.d \[len: [0-9]+\]\) map\(release:myvar\.arrcomp \[len: [0-9]+\]\) map\(detach:myvar\.arrcomp\.data \[bias: 0\]\) map\(release:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\)} 1 "gimple" } }


!$omp target enter data map(always, present, to: myvar)

! { dg-final { scan-tree-dump-times {map\(struct:myvar \[len: 4\]\) map\(to:myvar\.arrcomp \[pointer set, len: [0-9]+\]\) map\(force_present:myvar\.b \[len: [0-9]+\]\) map\(force_present:myvar\.c \[len: [0-9]+\]\) map\(always,present,to:myvar\.d \[len: [0-9]+\]\) map\(always,present,to:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\) map\(attach:myvar\.arrcomp\.data \[bias: 0\]\)} 1 "gimple" } }

!$omp target exit data map(always, present, from: myvar)

! { dg-final { scan-tree-dump-times {map\(release:myvar\.b \[len: [0-9]+\]\) map\(always,present,from:myvar\.c \[len: [0-9]+\]\) map\(always,present,from:myvar\.d \[len: [0-9]+\]\) map\(release:myvar\.arrcomp \[len: [0-9]+\]\) map\(detach:myvar\.arrcomp\.data \[bias: 0\]\) map\(release:MEM <integer\(kind=4\)\[0:\]> \[\(integer\(kind=4\)\[0:\] \*\)_[0-9]+\] \[len: _[0-9]+\]\)} 1 "gimple" } }

end
