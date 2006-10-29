/* { dg-do assemble } */

float testlf (float x)
{
  return __builtin_truncf (x);
}
double testl (double x)
{
  return __builtin_trunc (x);
}
long double testll (long double x)
{
  return __builtin_truncl (x);
}
