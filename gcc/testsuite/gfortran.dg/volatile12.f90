! { dg-do compile }
! { dg-options "-fdump-tree-optimized -O3" }
!
! PR fortran/45742
!

subroutine sub(arg)
  integer, volatile :: arg
  if (arg /= arg) call I_dont_exist()
end

! { dg-final { scan-tree-dump "integer.kind=.. . volatile arg" "optimized" } }
! { dg-final { scan-tree-dump-times " =.v. arg;" 2 "optimized" } }
! { dg-final { scan-tree-dump "i_dont_exist" "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }

