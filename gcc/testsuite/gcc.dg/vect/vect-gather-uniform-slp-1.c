#include "tree-vect.h"

#define N 64

int table[16] = { 1, 2, 3, 4, 5, 6, 7, 8,
		  9, 10, 11, 12, 13, 14, 15, 16 };
int y[N * 2];

/* Two gathers from the same base that form one SLP group.  i & 4 and
   i & 8 are each uniform per 4-lane chunk, but the group's vector
   interleaves the offsets of both statements, so it must not be turned
   into a broadcast.  */

void __attribute__((noipa))
f (int *restrict y, int *restrict t, int n)
{
  for (int i = 0; i < n; ++i)
    {
      y[i * 2] = t[i & 4] + 1;
      y[i * 2 + 1] = t[i & 8] + 2;
    }
}

int
main (void)
{
  check_vect ();

  f (y, table, N);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (y[i * 2] != table[i & 4] + 1
	|| y[i * 2 + 1] != table[i & 8] + 2)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
