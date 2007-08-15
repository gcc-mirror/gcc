! PR middle-end/33074
! { dg-do compile }
! { dg-options "-O" }

subroutine pr33074(a, w)
  real a(1), w(1)
  a(1) = 2.0**int(w(1))
end
