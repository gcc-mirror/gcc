/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcombine" } */

#include <limits.h>

_Bool or1(unsigned x, unsigned y)
{
  /* x <= y || x == 0 --> x <= y */
  return x <= y || x == 0;
}

_Bool or2(unsigned x, unsigned y)
{
  /* x >= y || x == UINT_MAX --> x >= y */
  return x >= y || x == UINT_MAX;
}

_Bool or3(signed x, signed y)
{
  /* x <= y || x == INT_MIN --> x <= y */
  return x <= y || x == INT_MIN;
}

_Bool or4(signed x, signed y)
{
  /* x >= y || x == INT_MAX --> x >= y */
  return x >= y || x == INT_MAX;
}

/* { dg-final { scan-tree-dump-not " == " "ifcombine" } } */
