! PR middle-end/82004
! { dg-do run }
! { dg-options "-Ofast" }

  integer, parameter :: r8 = selected_real_kind(13), i4 = kind(1)
  integer (i4), parameter :: a = 400, b = 2
  real (r8), parameter, dimension(b) :: c = (/ .001_r8, 10.00_r8 /)
  real (r8) :: d, e, f, g, h
  real (r8), parameter :: j &
    = 10**(log10(c(1))-(log10(c(b))-log10(c(1)))/real(a))

  d = c(1)
  e = c(b)
  f = (log10(e)-log10(d))/real(a)
  g = log10(d) - f
  h = 10**(g)
  if (h.ne.j) stop 1
end
