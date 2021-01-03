/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */
/* PR tree-optimization/67682.  */

#include "tree-vect.h"

int __attribute__((__aligned__(8))) a[8];
int __attribute__((__aligned__(8))) b[8];

__attribute__ ((noinline)) void
test ()
{
    a[0] = b[0] + 1;
    a[1] = b[1] + 2;
    a[2] = b[2] + 3;
    a[3] = b[3] + 4;
    a[4] = b[0] * 3;
    a[5] = b[2] * 4;
    a[6] = b[4] * 5;
    a[7] = b[6] * 7;
}

int
main (int argc, char **argv)
{
  check_vect ();

  for (int i = 0; i < 8; i++)
    a[i] = 1;
  for (int i = 0; i < 8; i++)
    b[i] = i + 4;
  __asm__ volatile ("" : : : "memory");
  test (a, b);
  __asm__ volatile ("" : : : "memory");
  if ((a[0] != 5) || (a[1] != 7) || (a[2] != 9) || (a[3] != 11)
      || (a[4] != 12) || (a[5] != 24) || (a[6] != 40) || (a[7] != 70))
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Basic block will be vectorized using SLP" 1 "slp2" } } */
/* Because we disable the cost model, targets with variable-length
   vectors can end up vectorizing the store to a[0..7] on its own.
   With the cost model we do something sensible.  */
/* { dg-final { scan-tree-dump-times "optimized: basic block" 2 "slp2" { xfail vect_variable_length } } } */
