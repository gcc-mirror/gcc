/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcombine" } */

#include <limits.h>

_Bool and1(unsigned x, unsigned y)
{
  /* x > y && x == 0 --> false */
  return x > y && x == 0;
}

_Bool and2(unsigned x, unsigned y)
{
  /* x < y && x == UINT_MAX --> false */
  return x < y && x == UINT_MAX;
}

_Bool and3(signed x, signed y)
{
  /* x > y && x == INT_MIN --> false */
  return x > y && x == INT_MIN;
}

_Bool and4(signed x, signed y)
{
  /* x < y && x == INT_MAX --> false */
  return x < y && x == INT_MAX;
}

/* { dg-final { scan-tree-dump-not " == " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not " > " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not " < " "ifcombine" } } */
