// { dg-do run { target c++20 } }
// { dg-require-effective-target inf }
// { dg-options "-O2 -g" }

#include "pr94589-5.C"

A bool f1 (double i, double j) { auto c = i <=> j; return c == 0; }
A bool f2 (double i, double j) { auto c = i <=> j; return c != 0; }
A bool f9 (double i, double j) { auto c = i <=> j; return c == std::partial_ordering::equivalent; }
A bool f10 (double i, double j) { auto c = i <=> j; return c != std::partial_ordering::equivalent; }
A bool f13 (double i) { auto c = i <=> 5.0; return c == 0; }
A bool f14 (double i) { auto c = i <=> 5.0; return c != 0; }
A bool f21 (double i) { auto c = i <=> 5.0; return c == std::partial_ordering::equivalent; }
A bool f22 (double i) { auto c = i <=> 5.0; return c != std::partial_ordering::equivalent; }
A bool f25 (double i, double j) { auto c = i <=> j; return c == std::partial_ordering::unordered; }
A bool f26 (double i, double j) { auto c = i <=> j; return c != std::partial_ordering::unordered; }
A bool f27 (double i) { auto c = i <=> 5.0; return c == std::partial_ordering::unordered; }
A bool f28 (double i) { auto c = i <=> 5.0; return c != std::partial_ordering::unordered; }

#define C(fn, i, j, r) if (fn (i, j) != r) __builtin_abort ()
#define D(fn, i, r) if (fn (i) != r) __builtin_abort ()

int
main ()
{
  C (f1, 7.0, 8.0, false);
  C (f1, 8.0, 8.0, true);
  C (f1, 9.0, 8.0, false);
  C (f1, __builtin_nan (""), 8.0, false);
  C (f2, 7.0, 8.0, true);
  C (f2, 8.0, 8.0, false);
  C (f2, 9.0, 8.0, true);
  C (f2, __builtin_nan (""), 8.0, true);
  C (f3, 7.0, 8.0, false);
  C (f3, 8.0, 8.0, false);
  C (f3, 9.0, 8.0, true);
  C (f3, __builtin_nan (""), 8.0, false);
  C (f4, 7.0, 8.0, true);
  C (f4, 8.0, 8.0, false);
  C (f4, 9.0, 8.0, false);
  C (f4, __builtin_nan (""), 8.0, false);
  C (f5, 7.0, 8.0, false);
  C (f5, 8.0, 8.0, true);
  C (f5, 9.0, 8.0, true);
  C (f5, __builtin_nan (""), 8.0, false);
  C (f6, 7.0, 8.0, true);
  C (f6, 8.0, 8.0, true);
  C (f6, 9.0, 8.0, false);
  C (f6, __builtin_nan (""), 8.0, false);
  C (f7, 7.0, 8.0, true);
  C (f7, 8.0, 8.0, false);
  C (f7, 9.0, 8.0, false);
  C (f7, __builtin_nan (""), 8.0, false);
  C (f8, 7.0, 8.0, false);
  C (f8, 8.0, 8.0, true);
  C (f8, 9.0, 8.0, true);
  C (f8, __builtin_nan (""), 8.0, true);
  C (f9, 7.0, 8.0, false);
  C (f9, 8.0, 8.0, true);
  C (f9, 9.0, 8.0, false);
  C (f9, __builtin_nan (""), 8.0, false);
  C (f10, 7.0, 8.0, true);
  C (f10, 8.0, 8.0, false);
  C (f10, 9.0, 8.0, true);
  C (f10, __builtin_nan (""), 8.0, true);
  C (f11, 7.0, 8.0, false);
  C (f11, 8.0, 8.0, false);
  C (f11, 9.0, 8.0, true);
  C (f11, __builtin_nan (""), 8.0, false);
  C (f12, 7.0, 8.0, true);
  C (f12, 8.0, 8.0, true);
  C (f12, 9.0, 8.0, false);
  C (f12, __builtin_nan (""), 8.0, true);
  D (f13, 4.0, false);
  D (f13, 5.0, true);
  D (f13, 6.0, false);
  D (f13, __builtin_nan (""), false);
  D (f14, 4.0, true);
  D (f14, 5.0, false);
  D (f14, 6.0, true);
  D (f14, __builtin_nan (""), true);
  D (f15, 4.0, false);
  D (f15, 5.0, false);
  D (f15, 6.0, true);
  D (f15, __builtin_nan (""), false);
  D (f16, 4.0, true);
  D (f16, 5.0, false);
  D (f16, 6.0, false);
  D (f16, __builtin_nan (""), false);
  D (f17, 4.0, false);
  D (f17, 5.0, true);
  D (f17, 6.0, true);
  D (f17, __builtin_nan (""), false);
  D (f18, 4.0, true);
  D (f18, 5.0, true);
  D (f18, 6.0, false);
  D (f18, __builtin_nan (""), false);
  D (f19, 4.0, true);
  D (f19, 5.0, false);
  D (f19, 6.0, false);
  D (f19, __builtin_nan (""), false);
  D (f20, 4.0, false);
  D (f20, 5.0, true);
  D (f20, 6.0, true);
  D (f20, __builtin_nan (""), true);
  D (f21, 4.0, false);
  D (f21, 5.0, true);
  D (f21, 6.0, false);
  D (f21, __builtin_nan (""), false);
  D (f22, 4.0, true);
  D (f22, 5.0, false);
  D (f22, 6.0, true);
  D (f22, __builtin_nan (""), true);
  D (f23, 4.0, false);
  D (f23, 5.0, false);
  D (f23, 6.0, true);
  D (f23, __builtin_nan (""), false);
  D (f24, 4.0, true);
  D (f24, 5.0, true);
  D (f24, 6.0, false);
  D (f24, __builtin_nan (""), true);
  C (f25, 7.0, 8.0, false);
  C (f25, 8.0, 8.0, false);
  C (f25, 9.0, 8.0, false);
  C (f25, __builtin_nan (""), 8.0, true);
  C (f26, 7.0, 8.0, true);
  C (f26, 8.0, 8.0, true);
  C (f26, 9.0, 8.0, true);
  C (f26, __builtin_nan (""), 8.0, false);
  D (f27, 4.0, false);
  D (f27, 5.0, false);
  D (f27, 6.0, false);
  D (f27, __builtin_nan (""), true);
  D (f28, 4.0, true);
  D (f28, 5.0, true);
  D (f28, 6.0, true);
  D (f28, __builtin_nan (""), false);
}
