! { dg-additional-options "-fdump-tree-original" }

type t
integer, pointer :: arr(:)
end type t

type(t) :: var

allocate (var%arr(1:100))

!$omp target enter data map(to: var%arr(10:20))
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target enter data map\(to:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\)$} 1 "original" } }

!$omp target exit data map(release: var%arr(10:20))
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target exit data map\(release:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(release:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\)$} 1 "original" } }


!$omp target enter data map(alloc: var%arr(20:30))
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target enter data map\(alloc:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\)$} 1 "original" } }

!$omp target exit data map(delete: var%arr(20:30))
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target exit data map\(delete:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(delete:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\)$} 1 "original" } }


!$omp target enter data map(to: var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target enter data map\(to:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\] \[runtime_implicit\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: 0\]\)$} 1 "original" } }

!$omp target exit data map(release: var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target exit data map\(release:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\] \[runtime_implicit\]\) map\(release:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: 0\]\)$} 1 "original" } }


!$omp target enter data map(alloc: var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target enter data map\(alloc:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\] \[runtime_implicit\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: 0\]\)$} 1 "original" } }

!$omp target exit data map(delete: var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target exit data map\(delete:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\] \[runtime_implicit\]\) map\(delete:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[bias: 0\]\)$} 1 "original" } }


end
