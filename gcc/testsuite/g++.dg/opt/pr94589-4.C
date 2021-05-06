// { dg-do run { target c++20 } }
// { dg-options "-O2 -g -ffast-math" }

#include "pr94589-2.C"

#define C(fn, i, j, r) if (fn (i, j) != r) __builtin_abort ()
#define D(fn, i, r) if (fn (i) != r) __builtin_abort ()

int
main ()
{
  C (f1, 7.0, 8.0, false);
  C (f1, 8.0, 8.0, true);
  C (f1, 9.0, 8.0, false);
  C (f2, 7.0, 8.0, true);
  C (f2, 8.0, 8.0, false);
  C (f2, 9.0, 8.0, true);
  C (f3, 7.0, 8.0, false);
  C (f3, 8.0, 8.0, false);
  C (f3, 9.0, 8.0, true);
  C (f4, 7.0, 8.0, true);
  C (f4, 8.0, 8.0, false);
  C (f4, 9.0, 8.0, false);
  C (f5, 7.0, 8.0, false);
  C (f5, 8.0, 8.0, true);
  C (f5, 9.0, 8.0, true);
  C (f6, 7.0, 8.0, true);
  C (f6, 8.0, 8.0, true);
  C (f6, 9.0, 8.0, false);
  C (f7, 7.0, 8.0, true);
  C (f7, 8.0, 8.0, false);
  C (f7, 9.0, 8.0, false);
  C (f8, 7.0, 8.0, false);
  C (f8, 8.0, 8.0, true);
  C (f8, 9.0, 8.0, true);
  C (f9, 7.0, 8.0, false);
  C (f9, 8.0, 8.0, true);
  C (f9, 9.0, 8.0, false);
  C (f10, 7.0, 8.0, true);
  C (f10, 8.0, 8.0, false);
  C (f10, 9.0, 8.0, true);
  C (f11, 7.0, 8.0, false);
  C (f11, 8.0, 8.0, false);
  C (f11, 9.0, 8.0, true);
  C (f12, 7.0, 8.0, true);
  C (f12, 8.0, 8.0, true);
  C (f12, 9.0, 8.0, false);
  D (f13, 4.0, false);
  D (f13, 5.0, true);
  D (f13, 6.0, false);
  D (f14, 4.0, true);
  D (f14, 5.0, false);
  D (f14, 6.0, true);
  D (f15, 4.0, false);
  D (f15, 5.0, false);
  D (f15, 6.0, true);
  D (f16, 4.0, true);
  D (f16, 5.0, false);
  D (f16, 6.0, false);
  D (f17, 4.0, false);
  D (f17, 5.0, true);
  D (f17, 6.0, true);
  D (f18, 4.0, true);
  D (f18, 5.0, true);
  D (f18, 6.0, false);
  D (f19, 4.0, true);
  D (f19, 5.0, false);
  D (f19, 6.0, false);
  D (f20, 4.0, false);
  D (f20, 5.0, true);
  D (f20, 6.0, true);
  D (f21, 4.0, false);
  D (f21, 5.0, true);
  D (f21, 6.0, false);
  D (f22, 4.0, true);
  D (f22, 5.0, false);
  D (f22, 6.0, true);
  D (f23, 4.0, false);
  D (f23, 5.0, false);
  D (f23, 6.0, true);
  D (f24, 4.0, true);
  D (f24, 5.0, true);
  D (f24, 6.0, false);
}
