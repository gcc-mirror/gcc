! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O2 -fdump-tree-optimized" }

subroutine foo (a, b, c, d, e, f, g, h)
  real (kind=8) :: a, b, c, d, e, f, g, h
  a = min (a, b, c, d, e, f, g, h)
end subroutine


subroutine foof (a, b, c, d, e, f, g, h)
  real (kind=4) :: a, b, c, d, e, f, g, h
  a = min (a, b, c, d, e, f, g, h)
end subroutine

! { dg-final { scan-tree-dump-times "\.FMIN " 14 "optimized" } }
