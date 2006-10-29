/* { dg-do assemble } */

float testlf (float x)
{
  return __builtin_roundf (x);
}
double testl (double x)
{
  return __builtin_round (x);
}
long double testll (long double x)
{
  return __builtin_roundl (x);
}

