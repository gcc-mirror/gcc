/* { dg-do compile } */
/* { dg-require-effective-target vect_int_mult } */

int
bar (int *x, int a, int b, int n)
{
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  int sum1 = 0;
  int sum2 = 0;
  for (int i = 0; i < n; ++i)
    {
      /* Reduction chain vectorization fails here because of the
         different operations but we can still vectorize both
	 reductions as SLP reductions, saving IVs.  */
      sum1 += x[2*i] - a;
      sum1 += x[2*i+1] * b;
      sum2 += x[2*i] - b;
      sum2 += x[2*i+1] * a;
    }
  return sum1 + sum2;
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
