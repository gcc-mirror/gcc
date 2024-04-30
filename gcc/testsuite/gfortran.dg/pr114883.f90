! PR tree-optimization/114883
! { dg-do compile }
! { dg-options "-O2 -fvect-cost-model=cheap" }
! { dg-additional-options "-march=x86-64-v4" { target i?86-*-* x86_64-*-* } }

subroutine pr114883_1(a, b, c, d, e, f, g, h, o)
  real(8) :: c(1011), d(1011), e(0:1011)
  real(8) :: p, q, f, r, g(1011), h(1011), b, bar
  integer :: o(100), a, t, u
  p = 0.0_8
  r = bar()
  u = 1
  do i = 1,a
    do k = 1,1011
      km1 = max0(k-1,1)
      h(k) = c(k) * e(k-1) * d(km1)
      f = g(k) + h(k)
      if(f.gt.1.e-6)then
        p = min(p,r)
      endif
    end do
    q = 0.9_8 * p
    t = integer(b/q + 1)
    if(t>100)then
      u = t
    endif
    o(u) = o(u) + 1
  end do
end subroutine pr114883_1
subroutine pr114883_2(a, b, c, d, e, f, g, h, o)
  real(8) :: c(1011), d(1011), e(0:1011)
  real(8) :: p, q, f, r, g(1011), h(1011), b, bar
  integer :: o(100), a, t, u
  p = 0.0_8
  r = bar()
  u = 1
  do i = 1,a
    do k = 1,1011
      km1 = max0(k-1,1)
      h(k) = c(k) * e(k-1) * d(km1)
      f = g(k) + h(k)
      if(f.gt.1.e-6)then
        p = max(p,r)
      endif
    end do
    q = 0.9_8 * p
    t = integer(b/q + 1)
    if(t>100)then
      u = t
    endif
    o(u) = o(u) + 1
  end do
end subroutine pr114883_2
