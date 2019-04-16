! { dg-do compile }

function f1 (a, b, c, d, e, f) ! { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } }
  integer, value :: a, b, c
  integer :: d, e, f, f1
!$omp declare simd (f1) uniform(b) linear(c, d) linear(uval(e)) linear(ref(f))
  a = a + 1
  b = b + 1
  c = c + 1
  d = d + 1
  e = e + 1
  f = f + 1
  f1 = a + b + c + d + e + f
end function f1
integer function f2 (a, b) ! { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } }
  integer :: a, b
!$omp declare simd uniform(b) linear(ref(a):b)
  a = a + 1
  f2 = a + b
end function f2
