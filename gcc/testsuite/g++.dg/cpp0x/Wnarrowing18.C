// PR c++/94590 - Detect long double -> double narrowing.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-mlong-double-64" { target x86_64-*-* i?86-*-* } }

int
main ()
{
  using T = long double;
  extern long double ld;
  extern T ld2;
  extern const T ld3;
  double d{ld}; // { dg-error "narrowing conversion" }
  double d2{ld2}; // { dg-error "narrowing conversion" }
  double d3{ld3}; // { dg-error "narrowing conversion" }
}
