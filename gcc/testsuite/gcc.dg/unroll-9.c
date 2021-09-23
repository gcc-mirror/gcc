/* { dg-options "-O3 -fdump-tree-unrolljam -fno-math-errno" } */

void
f (float *restrict x, float y[100][100])
{
  for (int j = 0; j < 100; ++j)
    for (int i = 0; i < 100; ++i)
      x[i] += __builtin_expf (y[j][i]);
}

/* The loop should be unrolled 2 times, without a tail loop.  */
/* { dg-final { scan-tree-dump-times "__builtin_expf" 2 "unrolljam" } } */
