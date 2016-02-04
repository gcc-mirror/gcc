! { dg-do compile }
subroutine foo(u, n, fl)
  implicit none
  integer n
  real u(5, n), fl(5,n), wl(5,n)
  real c
  c = 1
  wl = u
  fl = cshift(c * wl, 1, 2)
end subroutine foo
