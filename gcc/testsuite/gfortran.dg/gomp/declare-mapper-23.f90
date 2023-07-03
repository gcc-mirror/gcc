! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-gimple" }

type t
integer :: a, b, c, d
end type t

type(t) :: myvar

!$omp declare mapper (t :: x) map(to: x%a) map(alloc: x%b) &
!$omp &                       map(from: x%c) map(tofrom: x%d)

!$omp target data map(to: myvar)

! { dg-final { scan-tree-dump-times {map\(struct:myvar \[len: 4\]\) map\(to:myvar\.a \[len: [0-9]+\]\) map\(alloc:myvar\.b \[len: [0-9]+\]\) map\(alloc:myvar\.c \[len: [0-9]+\]\) map\(to:myvar\.d \[len: [0-9]+\]\)} 1 "gimple" } }

!$omp end target data

!$omp target data map(alloc: myvar)

! { dg-final { scan-tree-dump-times {map\(struct:myvar \[len: 4\]\) map\(alloc:myvar\.a \[len: [0-9]+\]\) map\(alloc:myvar\.b \[len: [0-9]+\]\) map\(alloc:myvar\.c \[len: [0-9]+\]\) map\(alloc:myvar\.d \[len: [0-9]+\]\)} 1 "gimple" } }

!$omp end target data

end
