/* { dg-additional-options "-std=gnu17" } */
/* { dg-require-effective-target vect_int } */
/* PR tree-optimization/67682.  */

#include "tree-vect.h"

int __attribute__((__aligned__(8))) a[8];
int __attribute__((__aligned__(8))) b[4];

__attribute__ ((noinline)) void
test ()
{
    a[0] = b[0];
    a[1] = b[1];
    a[2] = b[2];
    a[3] = b[3];
    a[4] = 0;
    a[5] = 0;
    a[6] = 0;
    a[7] = 0;
}

int
main (int argc, char **argv)
{
  check_vect ();

  for (int i = 0; i < 8; i++)
    a[i] = 1;
  for (int i = 0; i < 4; i++)
    b[i] = i + 4;
  __asm__ volatile ("" : : : "memory");
  test (a, b);
  __asm__ volatile ("" : : : "memory");
#pragma GCC novector
  for (int i = 0; i < 4; i++)
    if (a[i] != i+4)
      abort ();
#pragma GCC novector
  for (int i = 4; i < 8; i++)
    if (a[i] != 0)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Basic block will be vectorized using SLP" 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times "optimized: basic block" 2 "slp2" } } */
