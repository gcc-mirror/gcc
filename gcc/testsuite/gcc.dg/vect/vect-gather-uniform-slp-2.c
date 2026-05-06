#include "tree-vect.h"

#define N 64

int table[16] = { 1, 2, 3, 4, 5, 6, 7, 8,
		  9, 10, 11, 12, 13, 14, 15, 16 };
int y[N];

/* Two gathers with offsets i & 4 and (i + 1) & 4, i stepping by two.
   They are not uniform (the step is two) and are not grouped; this
   checks that the result is correct.  */

void __attribute__((noipa))
f (int *restrict y, int *restrict t, int n)
{
  for (int i = 0; i < n; i += 2)
    {
      y[i] = t[i & 4];
      y[i + 1] = t[(i + 1) & 4];
    }
}

int
main (void)
{
  check_vect ();

  f (y, table, N);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (y[i] != table[i & 4])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
