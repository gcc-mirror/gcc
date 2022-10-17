/* { dg-options "-O -fdump-tree-optimized" } */

#include <arm_sve.h>

unsigned int
foo (unsigned int x)
{
  unsigned long tmp = x;
  tmp += svcntb ();
  x = tmp;
  return x - svcntb ();
}

/* { dg-final { scan-tree-dump-not { POLY_INT_CST } optimized } } */
