! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }

subroutine foo (a, b, c, d, e, f, g, h)
  integer (kind=4) :: a, b, c, d, e, f, g, h
  a = min (a, b, c, d, e, f, g, h)
end subroutine

subroutine foof (a, b, c, d, e, f, g, h)
  integer (kind=4) :: a, b, c, d, e, f, g, h
  a = max (a, b, c, d, e, f, g, h)
end subroutine

! { dg-final { scan-tree-dump-times "MIN_EXPR" 7 "optimized" } }
! { dg-final { scan-tree-dump-times "MAX_EXPR" 7 "optimized" } }
