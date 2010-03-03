! PR debug/43229
! { dg-do compile }
! { dg-options "-g -O3 -ffast-math" }
! { dg-options "-g -O3 -ffast-math -msse3" { target { i?86-*-* x86_64-*-* } } }

function foo (c, d)
  real(8) :: c(6), d(6), foo
  x = sum (c * d)
  foo = exp (-x)
end function foo
