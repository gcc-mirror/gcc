! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }

subroutine foo(x)
  integer :: x(4)
  x(:) = (/ 3, 1, 4, 1 /)
end subroutine

subroutine bar(x)
  integer :: x(4)
  x = (/ 3, 1, 4, 1 /)
end subroutine

! { dg-final { scan-tree-dump-times "data" 0 "original" } }
