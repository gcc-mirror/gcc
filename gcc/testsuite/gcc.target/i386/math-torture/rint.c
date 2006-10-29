/* { dg-do assemble } */

float testlf (float x)
{
  return __builtin_rintf (x);
}
double testl (double x)
{
  return __builtin_rint (x);
}
long double testll (long double x)
{
  return __builtin_rintl (x);
}

