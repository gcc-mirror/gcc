! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/95837
!
type t
  character(len=:, kind=4), pointer :: str2
end type t
type(t) :: var

allocate(character(len=5, kind=4) :: var%str2)

var%str2(1:1) = 4_"d"
var%str2(2:3) = 4_"ef"
var%str2(4:4) = achar(int(Z'1F600'), kind=4)
var%str2(5:5) = achar(int(Z'1F608'), kind=4)

if (var%str2(1:3) /= 4_"def") stop 1
if (ichar(var%str2(4:4)) /= int(Z'1F600')) stop 2
if (ichar(var%str2(5:5)) /= int(Z'1F608')) stop 2

deallocate(var%str2)
end

! Note: the last '\x00' is regarded as string terminator, hence, the trailing \0 byte is not in the dump

! { dg-final { scan-tree-dump {  \(\*var\.str2\)\[1\]{lb: 1 sz: 4} = "(d\\x00\\x00|\\x00\\x00\\x00d)"\[1\]{lb: 1 sz: 4};} "original" } }
! { dg-final { scan-tree-dump {  __builtin_memmove \(\(void \*\) &\(\*var.str2\)\[2\]{lb: 1 sz: 4}, \(void \*\) &"(e\\x00\\x00\\x00f\\x00\\x00|\\x00\\x00\\x00e\\x00\\x00\\x00f)"\[1\]{lb: 1 sz: 4}, 8\);} "original" } }
! { dg-final { scan-tree-dump {  \(\*var.str2\)\[4\]{lb: 1 sz: 4} = "(\\x00\\xf6\\x01|\\x00\\x01\\xf6)"\[1\]{lb: 1 sz: 4};} "original" } }
! { dg-final { scan-tree-dump {  \(\*var.str2\)\[5\]{lb: 1 sz: 4} = "(\\b\\xf6\\x01|\\x00\\x01\\xf6\\b)"\[1\]{lb: 1 sz: 4};} "original" } }
