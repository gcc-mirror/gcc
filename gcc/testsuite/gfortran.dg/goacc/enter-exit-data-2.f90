! { dg-additional-options "-fdump-tree-original" }

type t
integer, pointer :: arr(:)
end type t

type(t) :: var

allocate (var%arr(1:100))

!$acc enter data copyin(var%arr(10:20))
! { dg-final { scan-tree-dump-times {(?n)#pragma acc enter data map\(to:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D.[0-9]+ \* [0-9]+\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\);$} 1 "original" } }

!$acc exit data delete(var%arr(10:20))
! { dg-final { scan-tree-dump-times {(?n)#pragma acc exit data map\(release:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(release:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\);$} 1 "original" } }


!$acc enter data create(var%arr(20:30))
! { dg-final { scan-tree-dump-times {(?n)#pragma acc enter data map\(alloc:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\);$} 1 "original" } }

!$acc exit data finalize delete(var%arr(20:30))
! { dg-final { scan-tree-dump-times {(?n)#pragma acc exit data map\(release:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) parm\.[0-9]+\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(release:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: \(integer\(kind=[0-9]+\)\) parm\.[0-9]+\.data - \(integer\(kind=[0-9]+\)\) var\.arr\.data\]\) finalize;$} 1 "original" } }


!$acc enter data copyin(var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma acc enter data map\(to:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: 0\]\);$} 1 "original" } }

!$acc exit data delete(var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma acc exit data map\(release:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(release:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: 0\]\);$} 1 "original" } }


!$acc enter data create(var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma acc enter data map\(alloc:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(to:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: 0\]\);$} 1 "original" } }

!$acc exit data finalize delete(var%arr)
! { dg-final { scan-tree-dump-times {(?n)#pragma acc exit data map\(release:\*\(integer\(kind=[0-9]+\)\[0:\] \*\) var\.arr\.data \[len: D\.[0-9]+ \* [0-9]+\]\) map\(release:var\.arr \[pointer set, len: [0-9]+\]\) map\(attach_detach:var\.arr\.data \[bias: 0\]\) finalize;$} 1 "original" } }

end
