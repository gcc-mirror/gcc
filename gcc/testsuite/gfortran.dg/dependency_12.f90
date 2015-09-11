! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a,b)
  integer, pointer, dimension (:,:) :: a
  real, dimension(:,:) :: b

  where (a == 0)
    b = 0.0
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
