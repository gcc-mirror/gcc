! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/107508
!
use iso_c_binding
implicit none
character(len=:,kind=4), allocatable, target :: a4str(:), a4str2
type(c_ptr) :: cptr, cptr2

allocate(character(len=7,kind=4) :: a4str(-2:3))
allocate(character(len=9,kind=4) :: a4str2)

cptr = c_loc(a4str)
cptr2 = c_loc(a4str2)

if (len(a4str) /= 7) error stop
if (lbound(a4str,1) /= -2) error stop
if (ubound(a4str,1) /= 3) error stop
if (len(a4str2) /= 9) error stop

a4str = [4_"sf456aq", 4_"3dtzu24", 4_"_4fh7sm", 4_"=ff85s7", 4_"j=8af4d", 4_".,A%Fsz"]
a4str2 = 4_"4f5g5f8a9"

!print *, lbound(a4str), ubound(a4str)  ! expected (-2:3) - actually: (1:6)

if (len(a4str) /= 7) error stop
if (lbound(a4str,1) /= -2) error stop
if (ubound(a4str,1) /= 3) error stop
if (len(a4str2) /= 9) error stop
if (.not. c_associated (cptr, c_loc(a4str))) error stop
if (.not. c_associated (cptr2, c_loc(a4str2))) error stop
end

! { dg-final { scan-tree-dump-times "__builtin_malloc" 4 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_realloc" 2 "original" } }

! { dg-final { scan-tree-dump-times "a4str.data = __builtin_malloc \\(168\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "a4str.data = __builtin_realloc \\(a4str.data, 168\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "a4str2 = \\(character\\(kind=4\\)\\\[1:.a4str2\\\] \\*\\) __builtin_malloc \\(36\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "a4str2 = \\(character\\(kind=4\\)\\\[1:.a4str2\\\] \\*\\) __builtin_realloc \\(\\(void \\*\\) a4str2, 36\\);" 1 "original" } }

! Array: Assert, realloc-check assign string length (alloc + (realloc'ed) assignment):
! { dg-final { scan-tree-dump-times "if \\(\[^\\n\\r\]*\\.a4str != 7\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "if \\(D\\.\[0-9\]+ != 28\\) goto L\\." 1 "original" } }
! { dg-final { scan-tree-dump-times "\\.a4str = 7;" 2 "original" } }

! Scalar: Assert, realloc-check assign string length (alloc + (realloc'ed) assignment):
! { dg-final { scan-tree-dump-times "if \\(\[^\\n\\r\]*\\.a4str2 != 9\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "if \\(\\.a4str2 == 9\\) goto L\\." 1 "original" } }
! { dg-final { scan-tree-dump-times "\\.a4str2 = 9;" 2 "original" } }
