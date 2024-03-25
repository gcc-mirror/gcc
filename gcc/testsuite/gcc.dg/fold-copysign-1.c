/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

double foo (double x)
{
  double one = 1.;
  return __builtin_copysign (x, one);
}
double bar (double x)
{
  double minuszero = -0.;
  return __builtin_copysign (x, minuszero);
}

/* { dg-final { scan-tree-dump-times "__builtin_copysign" 1 "cddce1" } } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 1 "cddce1" } } */
