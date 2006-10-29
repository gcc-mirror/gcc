/* { dg-do assemble } */

float testlf (float x)
{
  return __builtin_ceilf (x);
}
double testl (double x)
{
  return __builtin_ceil (x);
}
long double testll (long double x)
{
  return __builtin_ceill (x);
}

