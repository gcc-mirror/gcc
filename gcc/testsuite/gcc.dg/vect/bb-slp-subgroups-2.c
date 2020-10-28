/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */
/* PR tree-optimization/67682.  */

#include "tree-vect.h"

int __attribute__((__aligned__(8))) a[8];
int __attribute__((__aligned__(8))) b[4];

__attribute__ ((noinline)) void
test ()
{
    a[0] = b[2] + 1;
    a[1] = b[0] + 2;
    a[2] = b[1] + 3;
    a[3] = b[1] + 4;
    a[4] = b[3] * 3;
    a[5] = b[0] * 4;
    a[6] = b[2] * 5;
    a[7] = b[1] * 7;
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
  if ((a[0] != 7) || a[1] != 6 || (a[2] != 8) || (a[3] != 9)
      || (a[4] != 21) || (a[5] != 16) || (a[6] != 30) || (a[7] != 35))
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Basic block will be vectorized using SLP" 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp2" } } */
