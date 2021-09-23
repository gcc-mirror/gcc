/* { dg-options "-O3 -fdump-tree-unrolljam" } */

void
f (int *restrict x, int *restrict y, int z[restrict 100][100])
{
  for (int j = 0; j < 100; ++j)
    for (int i = 0; i < 100; ++i)
      x[i] += y[i] * z[j][i];
}

/* The loop should be unrolled 2 times, leaving one load from x,
   one load from y and 2 loads from z.  */
/* { dg-final { scan-tree-dump-times { = \(*\*} 4 "unrolljam" } } */
