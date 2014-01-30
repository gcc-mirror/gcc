/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-std=c++11 -O3 -fopt-info-vec-optimized -fdump-tree-original -fdump-tree-gimple" } */

/* PR other/33426 */
/* Testing whether #pragma ivdep is working.  */

#include <vector>

template<class T, class T2>
void Loop(T *b, T2 c) {
#pragma GCC ivdep
  for (auto &i : *b) {
    i *= *c;
  }
}

void foo(std::vector<int> *ar, int *b) {
 Loop<std::vector<int>, int*>(ar, b);
}

/* { dg-message "loop vectorized" "" { target *-*-* } 0 } */
/* FIXME:     dg-bogus " version\[^\n\r]* alias" "" { target *-*-* } 0  */
/* { dg-final { cleanup-tree-dump "vect" } } */

/* { dg-final { scan-tree-dump-times "ANNOTATE_EXPR " 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
/* { dg-final { scan-tree-dump-times "ANNOTATE " 1 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
