/* { dg-do assemble } */

float testlf (float x)
{
  return __builtin_floorf (x);
}
double testl (double x)
{
  return __builtin_floor (x);
}
long double testll (long double x)
{
  return __builtin_floorl (x);
}

