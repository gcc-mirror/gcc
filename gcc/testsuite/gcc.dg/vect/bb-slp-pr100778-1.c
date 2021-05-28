/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double foo (int x, double *p)
{
  double res = p[0] + p[1];
  double tem = p[0] / x;
  if (x)
    {
      p[0] = tem;
      p[1] /= x;
    }
  return res + tem;
}

/* We may not SLP vectorize the FP division because it can trap and it
   is distributed between two basic-blocks.  */
/* { dg-final { scan-tree-dump "Build SLP failed: different BB for PHI or possibly trapping operation in _\[0-9\]+ = _\[0-9\]+ / _\[0-9\]+;" "slp2" } } */
