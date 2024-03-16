/* { dg-do compile } */

void
f (int *restrict y, int *restrict x, int *restrict indices, int *restrict cond, int n)
{
  for (int i = 0; i < n; ++i)
    {
      if (cond[i * 2])
	y[i * 2] = x[indices[i * 2]] + 1;
      if (cond[i * 2 + 1])
	y[i * 2 + 1] = x[indices[i * 2 + 1]] + 2;
    }
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" vect { target vect_gather_load_ifn } } } */
