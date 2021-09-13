// { dg-do run { target c++20 } }
// { dg-options "-O2 -g" }

#include "pr94589-1.C"

#define C(fn, i, j, r) if (fn (i, j) != r) __builtin_abort ()
#define D(fn, i, r) if (fn (i) != r) __builtin_abort ()

int
main ()
{
  C (f1, 7, 8, false);
  C (f1, 8, 8, true);
  C (f1, 9, 8, false);
  C (f2, 7, 8, true);
  C (f2, 8, 8, false);
  C (f2, 9, 8, true);
  C (f3, 7, 8, false);
  C (f3, 8, 8, false);
  C (f3, 9, 8, true);
  C (f4, 7, 8, true);
  C (f4, 8, 8, false);
  C (f4, 9, 8, false);
  C (f5, 7, 8, false);
  C (f5, 8, 8, true);
  C (f5, 9, 8, true);
  C (f6, 7, 8, true);
  C (f6, 8, 8, true);
  C (f6, 9, 8, false);
  C (f7, 7, 8, true);
  C (f7, 8, 8, false);
  C (f7, 9, 8, false);
  C (f8, 7, 8, false);
  C (f8, 8, 8, true);
  C (f8, 9, 8, true);
  C (f9, 7, 8, false);
  C (f9, 8, 8, true);
  C (f9, 9, 8, false);
  C (f10, 7, 8, true);
  C (f10, 8, 8, false);
  C (f10, 9, 8, true);
  C (f11, 7, 8, false);
  C (f11, 8, 8, false);
  C (f11, 9, 8, true);
  C (f12, 7, 8, true);
  C (f12, 8, 8, true);
  C (f12, 9, 8, false);
  D (f13, 4, false);
  D (f13, 5, true);
  D (f13, 6, false);
  D (f14, 4, true);
  D (f14, 5, false);
  D (f14, 6, true);
  D (f15, 4, false);
  D (f15, 5, false);
  D (f15, 6, true);
  D (f16, 4, true);
  D (f16, 5, false);
  D (f16, 6, false);
  D (f17, 4, false);
  D (f17, 5, true);
  D (f17, 6, true);
  D (f18, 4, true);
  D (f18, 5, true);
  D (f18, 6, false);
  D (f19, 4, true);
  D (f19, 5, false);
  D (f19, 6, false);
  D (f20, 4, false);
  D (f20, 5, true);
  D (f20, 6, true);
  D (f21, 4, false);
  D (f21, 5, true);
  D (f21, 6, false);
  D (f22, 4, true);
  D (f22, 5, false);
  D (f22, 6, true);
  D (f23, 4, false);
  D (f23, 5, false);
  D (f23, 6, true);
  D (f24, 4, true);
  D (f24, 5, true);
  D (f24, 6, false);
}
