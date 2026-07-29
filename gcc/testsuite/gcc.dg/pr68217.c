/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-vrp1 -fno-tree-ccp" } */

#include <limits.h>

/* Return x so that its range is still exported after the comparison folds
   away.  (x & LLONG_MIN) < 1 is true for both values x can take, so the
   test of it no longer keeps x live by itself.  */

long long foo (void)
{
    volatile int a = -1;
    long long b = LLONG_MIN;
    long long x = (a & b); // x == 0x8000000000000000
    if (x < 1LL) { ; } else { __builtin_abort(); }
    return x;
}

/* { dg-final { scan-tree-dump "\\\[-INF, -INF\\\]\\\[0, 0\\\]" "vrp1" } } */
