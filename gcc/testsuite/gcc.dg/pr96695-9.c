/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcombine" } */

#include <limits.h>

_Bool or1(unsigned *x, unsigned *y)
{
  /* x <= y || x != 0 --> true */
  return x <= y || x != 0;
}

_Bool or2(unsigned *x, unsigned *y)
{
  /* x >= y || x != -1 --> true */
  return x >= y || x != (unsigned*)-1;
}

/* { dg-final { scan-tree-dump-not " != " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not " <= " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not " >= " "ifcombine" } } */
