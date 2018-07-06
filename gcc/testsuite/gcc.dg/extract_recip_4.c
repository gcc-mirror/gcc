/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized-raw" } */

/* Don't expect any of these divisions to be extracted.  */
double f (double x, int p)
{
  if (p > 0)
    {
      return 1.0/(x * x);
    }

  if (p > -1)
    {
      return x * x * x;
    }
  return  1.0 /(x);
}

/* Expect a reciprocal to be extracted here.  */
double g (double *a, double x, double y)
{
  *a = 3 / y;
  double k = x / (y * y);

  if (y * y == 2.0)
    return k + 1 / y;
  else
    return k - 1 / y;
}

/* Expect 2 divisions in 'f' and 1 in 'g'.  */
/* { dg-final { scan-tree-dump-times "rdiv_expr" 3 "optimized" } } */
/* Expect 3 multiplications in 'f' and 4 in 'g'.  */
/* { dg-final { scan-tree-dump-times "mult_expr" 7 "optimized" } } */
