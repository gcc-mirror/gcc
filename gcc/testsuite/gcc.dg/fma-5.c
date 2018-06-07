/* { dg-options "-O2 -fdump-tree-optimized" } */

float
f1 (float a, float b, float c)
{
  return __builtin_fmaf (a, b, -c);
}

double
f2 (double a, double b, double c)
{
  return __builtin_fma (a, b, -c);
}

void
f3 (float a, float b, float c, float d, float e, float *res)
{
  res[0] = __builtin_fmaf (a, b, -e);
  res[1] = __builtin_fmaf (c, d, -e);
}

void
f4 (double a, double b, double c, double d, double e, double *res)
{
  res[0] = __builtin_fma (a, b, -e);
  res[1] = __builtin_fma (c, d, -e);
}

float
f5 (float a, float b, float c)
{
  return -__builtin_fmaf (-a, b, c);
}

double
f6 (double a, double b, double c)
{
  return -__builtin_fma (-a, b, c);
}

float
f7 (float a, float b, float c)
{
  return -__builtin_fmaf (a, -b, c);
}

double
f8 (double a, double b, double c)
{
  return -__builtin_fma (a, -b, c);
}

/* { dg-final { scan-tree-dump-times { = \.FMS \(} 10 "optimized" { target scalar_all_fma } } } */
