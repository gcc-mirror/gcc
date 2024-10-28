/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

/* PR tree-optimization/85605 */
#include <stdint.h>

template<class T,class T2>
inline bool cmp(T a, T2 b) {
  return a<0 ? true : T2(a) < b;
}

template<class T,class T2>
inline bool cmp2(T a, T2 b) {
  return (a<0) | (T2(a) < b);
}

bool f(int a, int b) {
    return cmp(int64_t(a), unsigned(b));
}

bool f2(int a, int b) {
    return cmp2(int64_t(a), unsigned(b));
}


/* Both of these functions should be optimized to the same, and have an | in them. */
/* { dg-final { scan-tree-dump-times " \\\| " 2 "optimized" } } */
