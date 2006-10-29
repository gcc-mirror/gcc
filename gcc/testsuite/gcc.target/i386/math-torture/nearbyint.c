/* { dg-do assemble } */

float testlf (float x)
{
  return __builtin_nearbyintf (x);
}
double testl (double x)
{
  return __builtin_nearbyint (x);
}
long double testll (long double x)
{
  return __builtin_nearbyintl (x);
}

