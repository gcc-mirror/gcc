/* { dg-do compile } */

#define N 16

void
f1 (int *restrict y, int *restrict x1, int *restrict x2,
    int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = x1[indices[i * 2]] + 1;
      y[i * 2 + 1] = x2[indices[i * 2 + 1]] + 2;
    }
}

void
f2 (int *restrict y, int *restrict x, int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = x[indices[i * 2]] + 1;
      y[i * 2 + 1] = x[indices[i * 2 + 1] * 2] + 2;
    }
}

void
f3 (int *restrict y, int *restrict x, int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = x[indices[i * 2]] + 1;
      y[i * 2 + 1] = x[(unsigned int) indices[i * 2 + 1]] + 2;
    }
}

/* { dg-final { scan-tree-dump-not "Loop contains only SLP stmts" vect { target vect_gather_load_ifn } } } */
