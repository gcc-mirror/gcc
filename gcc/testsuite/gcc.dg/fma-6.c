/* { dg-options "-O2 -fdump-tree-optimized" } */

float
f1 (float a, float b, float c)
{
  return __builtin_fmaf (-a, b, c);
}

double
f2 (double a, double b, double c)
{
  return __builtin_fma (-a, b, c);
}

float
f3 (float a, float b, float c)
{
  return __builtin_fmaf (a, -b, c);
}

double
f4 (double a, double b, double c)
{
  return __builtin_fma (a, -b, c);
}

void
f5 (float a, float b, float c, float d, float e, float *res)
{
  res[0] = __builtin_fmaf (-a, b, c);
  res[1] = __builtin_fmaf (-a, d, e);
}

void
f6 (double a, double b, double c, double d, double e, double *res)
{
  res[0] = __builtin_fma (-a, b, c);
  res[1] = __builtin_fma (-a, d, e);
}

void
f7 (float a, float b, float c, float d, float e, float *res)
{
  res[0] = __builtin_fmaf (a, -b, c);
  res[1] = __builtin_fmaf (d, -b, e);
}

void
f8 (double a, double b, double c, double d, double e, double *res)
{
  res[0] = __builtin_fma (a, -b, c);
  res[1] = __builtin_fma (d, -b, e);
}

float
f9 (float a, float b, float c)
{
  return -__builtin_fmaf (a, b, -c);
}

double
f10 (double a, double b, double c)
{
  return -__builtin_fma (a, b, -c);
}

/* { dg-final { scan-tree-dump-times { = \.FNMA \(} 14 "optimized" { target scalar_all_fma } } } */
