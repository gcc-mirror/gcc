/* PR tree-optimization/67815 */
/* { dg-do compile } */
/* { dg-options "-Ofast -g -fdump-tree-reassoc1-details" } */

extern float barf (float, float);
extern double bar (double, double);

float
f0 (float x)
{
  return 7.5 * __builtin_copysignf (2.0, x);
}

float
f1 (float x)
{
  return -7.5 * __builtin_copysignf (2.0, x);
}

double
f2 (double x, double y)
{
  return x * ((1.0/12) * __builtin_copysign (1.0, y));
}

double
f3 (double x, double y)
{
  return (x * (-1.0/12)) * __builtin_copysign (1.0, y);
}

double
f4 (double x, double y, double z)
{
  return (x * z) * ((1.0/12) * __builtin_copysign (4.0, y));
}

double
f5 (double x, double y, double z)
{
  return (x * (-1.0/12)) * z * __builtin_copysign (2.0, y);
}

float
f6 (float x, float y)
{
  return 7.5f * y * __builtin_copysignf (2.0f, x);
}

float
f7 (float x, float y)
{
  return -7.5f * y * __builtin_copysignf (2.0f, x);
}

float
f8 (float x)
{
  float tmp1 = 7.5f;
  float tmp2 = __builtin_copysignf (2.0f, x);
  return tmp1 * tmp2;
}

double
f9 (double x)
{
  double tmp1 = 7.5;
  double tmp2 = __builtin_copysign (2.0, x);
  return tmp1 * tmp2;
}

float
f10 (float x)
{
  float tmp1 = 7.5f;
  float tmp2 = __builtin_copysignf (2.0f, x);
  float tmp3 = tmp2 * 24.0f;
  return tmp1 * tmp2;
}

double
f11 (double x)
{
  double tmp1 = 7.5;
  double tmp2 = __builtin_copysign (2.0, x);
  double tmp3 = tmp2 * 24.0;
  return tmp1 * tmp2;
}

float
f12 (float x)
{
  float tmp1 = 7.5f;
  float tmp2 = __builtin_copysignf (2.0f, x);
  /* Can't reassoc here.  */
  return barf (tmp1 * tmp2, tmp2);
}

double
f13 (double x)
{
  double tmp1 = 7.5;
  double tmp2 = __builtin_copysign (2.0, x);
  /* Can't reassoc here.  */
  return bar (tmp1 * tmp2, tmp2);
}
/* { dg-final { scan-tree-dump-times "Optimizing copysign" 12 "reassoc1"} }*/
