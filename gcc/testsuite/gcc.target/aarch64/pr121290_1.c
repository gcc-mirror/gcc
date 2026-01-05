/* { dg-do compile } */
/* { dg-additional-options "-O3 -mcpu=neoverse-v2 -fdump-tree-vect-all -std=c99" } */

void
f (int *restrict x, int *restrict y, int *restrict z, int n)
{
  for (int i = 0; i < 4; ++i)
    {
      int res = 0;
      for (int j = 0; j < 100; ++j)
        res += y[j] * z[i];
      x[i] = res;
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "low throughput of per iteration due to splats" "vect" } } */
