/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-dce" } */

#include <stdint.h>

__attribute__((noinline, noipa))
void fun1(uint32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (x[i]&(~1)) != 0;
}

__attribute__((noinline, noipa, optimize("O0")))
void fun2(uint32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (x[i]&(~1)) != 0;
}

#include "bic-bitmask.h"

/* The loop guard (n & -16) > 0 now folds to n > 15, so anchor the scan on
   the statement end to keep it matching only the comparison under test.  */
/* { dg-final { scan-tree-dump-times {>\s* 1;} 1 dce7 { target vect_int } } } */
/* { dg-final { scan-tree-dump-not {&\s* 4294967294} dce7 { target vect_int } } } */

