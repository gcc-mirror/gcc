/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dce3" } */

#include <limits.h>

_Bool or1(unsigned *x, unsigned *y)
{
  /* x <= y || x == 0 --> x <= y */
  return x <= y || x == 0;
}

_Bool or2(unsigned *x, unsigned *y)
{
  /* x >= y || x == -1 --> x >= y */
  return x >= y || x == (unsigned*)-1;
}

/* { dg-final { scan-tree-dump-not " == " "dce3" } } */
