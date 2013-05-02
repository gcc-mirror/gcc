! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/57142
!
integer :: B(huge(1)+3_8,2_8)
integer(8) :: var1(2), var2, var3

var1 = shape(B,kind=8)
var2 = size(B,kind=8)
var3 = size(B,dim=1,kind=8)
end

! { dg-final { scan-tree-dump "static integer.kind=8. A..\\\[2\\\] = \\\{2147483650, 2\\\};" "original" } }
! { dg-final { scan-tree-dump "var2 = 4294967300;" "original" } }
! { dg-final { scan-tree-dump "var3 = 2147483650;" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
