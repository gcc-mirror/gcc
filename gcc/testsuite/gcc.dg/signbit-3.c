/* { dg-do assemble } */
/* { dg-options "-O1 --save-temps -fdump-tree-optimized" } */

#include <stdint.h>

void fun1(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (-x[i]) >> 31;
}

/* { dg-final { scan-tree-dump-times {\s+>\s+0;} 1 optimized } } */
/* { dg-final { scan-tree-dump-not {\s+>>\s+31} optimized } } */
