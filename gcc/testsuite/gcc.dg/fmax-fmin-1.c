/* { dg-options "-O -fdump-tree-optimized" } */

void
f1 (double *res, double x, double y)
{
  res[0] = __builtin_fmax (x, y);
  res[1] = __builtin_fmax (y, x);
}

void
f2 (double *res, double x, double y)
{
  res[0] = __builtin_fmin (x, y);
  res[1] = __builtin_fmin (y, x);
}

/* { dg-final { scan-tree-dump-times {__builtin_fmax} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {__builtin_fmin} 1 "optimized" } } */
