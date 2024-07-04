/* { dg-do compile } */

#define N 16

void
f1 (int *restrict y, int *restrict x1, int *restrict x2,
    int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = (indices[i * 2] < N * 2
		  ? x1[indices[i * 2]] + 1
		  : 1);
      y[i * 2 + 1] = (indices[i * 2 + 1] < N * 2
		      ? x2[indices[i * 2 + 1]] + 2
		      : 2);
    }
}

void
f2 (int *restrict y, int *restrict x, int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = (indices[i * 2] < N * 2
		  ? x[indices[i * 2]] + 1
		  : 1);
      y[i * 2 + 1] = (indices[i * 2 + 1] < N * 2
		      ? x[indices[i * 2 + 1] * 2] + 2
		      : 2);
    }
}

void
f3 (int *restrict y, int *restrict x, int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = (indices[i * 2] < N * 2
		  ? x[indices[i * 2]] + 1
		  : 1);
      y[i * 2 + 1] = (((unsigned int *)indices)[i * 2 + 1] < N * 2
		      ? x[((unsigned int *) indices)[i * 2 + 1]] + 2
		      : 2);
    }
}

/* We do not want to see a two-lane .MASK_LOAD or .MASK_GATHER_LOAD since
   the gathers are different on each lane.  This is a bit fragile and
   should possibly be turned into a runtime test.  */
/* { dg-final { scan-tree-dump-not "stmt 1 \[^\r\n\]* = .MASK" vect } } */
