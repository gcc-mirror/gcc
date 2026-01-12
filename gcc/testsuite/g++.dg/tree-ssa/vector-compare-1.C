// Even though arm sometimes has unaligned loads, memcmp is not transformed so disable
// the test for arm targets.
// { dg-do compile { target { c++11 && { non_strict_align && { ! { arm*-*-* } } } } } }
// { dg-options "-O2 -fdump-tree-optimized" }

// PR tree-optimization/116651

#include <vector>

bool test1(const std::vector<int>& in) {
    return in == std::vector<int>{24};
}

/* We should be to optimize this to:
   int *b = in.bptr;
   int *e = in.eptr;
   auto size = e - b;
   if (size != 4)
     return false;
   int v = *b;
   return v == 24;

*/

// { dg-final { scan-tree-dump-times "== 24" 1 "optimized" } } */
// { dg-final { scan-tree-dump-times "== 4" 1 "optimized"  { target int32 } } } */
