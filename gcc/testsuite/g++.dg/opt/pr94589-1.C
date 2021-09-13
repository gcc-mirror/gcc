// PR tree-optimization/94589
// { dg-do compile { target c++20 } }
// { dg-options "-O2 -g0 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times "\[ij]_\[0-9]+\\(D\\) (?:<|<=|==|!=|>|>=) \[ij]_\[0-9]+\\(D\\)" 12 "optimized" } }
// { dg-final { scan-tree-dump-times "i_\[0-9]+\\(D\\) (?:<|<=|==|!=|>|>=) \[45]" 12 "optimized" } }

#include <compare>

#define A __attribute__((noipa))
A bool f1 (int i, int j) { auto c = i <=> j; return c == 0; }
A bool f2 (int i, int j) { auto c = i <=> j; return c != 0; }
A bool f3 (int i, int j) { auto c = i <=> j; return c > 0; }
A bool f4 (int i, int j) { auto c = i <=> j; return c < 0; }
A bool f5 (int i, int j) { auto c = i <=> j; return c >= 0; }
A bool f6 (int i, int j) { auto c = i <=> j; return c <= 0; }
A bool f7 (int i, int j) { auto c = i <=> j; return c == std::strong_ordering::less; }
A bool f8 (int i, int j) { auto c = i <=> j; return c != std::strong_ordering::less; }
A bool f9 (int i, int j) { auto c = i <=> j; return c == std::strong_ordering::equal; }
A bool f10 (int i, int j) { auto c = i <=> j; return c != std::strong_ordering::equal; }
A bool f11 (int i, int j) { auto c = i <=> j; return c == std::strong_ordering::greater; }
A bool f12 (int i, int j) { auto c = i <=> j; return c != std::strong_ordering::greater; }
A bool f13 (int i) { auto c = i <=> 5; return c == 0; }
A bool f14 (int i) { auto c = i <=> 5; return c != 0; }
A bool f15 (int i) { auto c = i <=> 5; return c > 0; }
A bool f16 (int i) { auto c = i <=> 5; return c < 0; }
A bool f17 (int i) { auto c = i <=> 5; return c >= 0; }
A bool f18 (int i) { auto c = i <=> 5; return c <= 0; }
A bool f19 (int i) { auto c = i <=> 5; return c == std::strong_ordering::less; }
A bool f20 (int i) { auto c = i <=> 5; return c != std::strong_ordering::less; }
A bool f21 (int i) { auto c = i <=> 5; return c == std::strong_ordering::equal; }
A bool f22 (int i) { auto c = i <=> 5; return c != std::strong_ordering::equal; }
A bool f23 (int i) { auto c = i <=> 5; return c == std::strong_ordering::greater; }
A bool f24 (int i) { auto c = i <=> 5; return c != std::strong_ordering::greater; }
