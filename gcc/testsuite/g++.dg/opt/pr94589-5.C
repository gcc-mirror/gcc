// PR tree-optimization/94589
// { dg-do compile { target c++20 } }
// { dg-require-effective-target inf }
// { dg-options "-O2 -g0 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times "\[ij]_\[0-9]+\\(D\\) (?:<|<=|>|>=) \[ij]_\[0-9]+\\(D\\)" 8 "optimized" } }
// { dg-final { scan-tree-dump-times "i_\[0-9]+\\(D\\) (?:<|<=|>|>=) 5\\.0" 8 "optimized" } }

#include <compare>

#define A __attribute__((noipa))
A bool f3 (double i, double j) { auto c = i <=> j; return c > 0; }
A bool f4 (double i, double j) { auto c = i <=> j; return c < 0; }
A bool f5 (double i, double j) { auto c = i <=> j; return c >= 0; }
A bool f6 (double i, double j) { auto c = i <=> j; return c <= 0; }
A bool f7 (double i, double j) { auto c = i <=> j; return c == std::partial_ordering::less; }
A bool f8 (double i, double j) { auto c = i <=> j; return c != std::partial_ordering::less; }
A bool f11 (double i, double j) { auto c = i <=> j; return c == std::partial_ordering::greater; }
A bool f12 (double i, double j) { auto c = i <=> j; return c != std::partial_ordering::greater; }
A bool f15 (double i) { auto c = i <=> 5.0; return c > 0; }
A bool f16 (double i) { auto c = i <=> 5.0; return c < 0; }
A bool f17 (double i) { auto c = i <=> 5.0; return c >= 0; }
A bool f18 (double i) { auto c = i <=> 5.0; return c <= 0; }
A bool f19 (double i) { auto c = i <=> 5.0; return c == std::partial_ordering::less; }
A bool f20 (double i) { auto c = i <=> 5.0; return c != std::partial_ordering::less; }
A bool f23 (double i) { auto c = i <=> 5.0; return c == std::partial_ordering::greater; }
A bool f24 (double i) { auto c = i <=> 5.0; return c != std::partial_ordering::greater; }
