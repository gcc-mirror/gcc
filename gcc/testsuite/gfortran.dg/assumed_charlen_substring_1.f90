! { dg-do compile }
! { dg-options "-O -fdump-tree-original" }
! PR 51338 - this used to ICE.
! Original test case by Bud Davis.
subroutine foo(a,b)
  character(len=*) :: a
  if (a(1:) /= a(1:)) call do_not_use
end subroutine foo
! { dg-final { scan-tree-dump-times "do_not_use" 0 "original" } }
