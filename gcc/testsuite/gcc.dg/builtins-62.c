/* { dg-do compile } */
/* { dg-options "-O -ffinite-math-only -fdump-tree-optimized" } */
/* { dg-require-effective-target c99_runtime } */

double test1 (double x)
{
  double s, c;
  s = __builtin_sin (x);
  c = __builtin_cos (x);
  return s + c;
}

double test2 (double x)
{
  double s, c;
  x = x * 2;
  s = __builtin_sin (x);
  c = __builtin_cos (x);
  return s + c;
}

double test3 (double x, int b)
{
  double s, c;
  if (b)
    x = x * 2;
  s = __builtin_sin (x);
  c = __builtin_cos (x);
  return s + c;
}

double test4 (double x)
{
  double s;
  x = x * 2;
  s = __builtin_sin (x);
  return s;
}

/* { dg-final { scan-tree-dump-times "cexpi" 3 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
