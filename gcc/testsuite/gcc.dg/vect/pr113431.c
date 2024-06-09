/* { dg-additional-options "-O3 -fdump-tree-slp1-details" } */

#include "tree-vect.h"

int a[2][9];
int b;
int main()
{
  check_vect ();
  for (b = 0; b < 2; b++)
    for (long e = 8; e > 0; e--)
      a[b][e] = a[0][1] == 0;
  if (a[1][1] != 0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block part vectorized" 2 "slp1" { target { vect_int && vect_hw_misalign } } } } */
