/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

double __attribute__((optimize ("-frounding-math")))
frnd (double x, double y)
{
  return __builtin_fabs (x) * __builtin_fabs (y);
}

int __attribute__((optimize ("-fwrapv")))
wrap (int x, int y)
{
  return __builtin_abs (x) * __builtin_abs (y);
}

int __attribute__((optimize ("-ftrapv")))
trap (int x, int y)
{
  return __builtin_abs (x) * __builtin_abs (y);
}

int gx, gy;
int shared (int x, int y)
{
  int ax = __builtin_abs (x);
  int ay = __builtin_abs (y);
  gx = ax;
  gy = ay;
  return ax * ay;
}

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 8 "optimized" } } */
