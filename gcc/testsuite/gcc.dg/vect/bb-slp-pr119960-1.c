/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double foo (double *dst, double *src, int b)
{
  double y = src[1];
  if (b)
    {
      dst[0] = src[0];
      dst[1] = y;
    }
  return y;
}

/* { dg-final { scan-tree-dump "optimized: basic block part vectorized" "slp2" { target vect_double } } } */
