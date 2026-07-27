// PR tree-optimization/126296
// { dg-do compile { target c++20 } }
// { dg-options "-O2 -g0 -ftrapping-math -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times "\[ij]_\[0-9]+\\(D\\) <> \[ij]_\[0-9]+\\(D\\)" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "i_\[0-9]+\\(D\\) <> 5\\.0" 1 "optimized" } }
// { dg-final { scan-tree-dump-not "if " "optimized" } }

#include <compare>

#define A __attribute__((noipa))
A bool f1 (double i, double j)
{
  auto c = i <=> j;
  return (c < 0) | (c > 0);
}
A bool f2 (double i)
{
  auto c = i <=> 5.0;
  return (c < 0) | (c > 0);
}
