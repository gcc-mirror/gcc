/* { dg-do assemble } */

long testlf (float x)
{
  return __builtin_lfloorf (x);
}
long testl (double x)
{
  return __builtin_lfloor (x);
}
long testll (long double x)
{
  return __builtin_lfloorl (x);
}
long long testllf (float x)
{
  return __builtin_llfloorf (x);
}
long long testll_ (double x)
{
  return __builtin_llfloor (x);
}
long long testlll (long double x)
{
  return __builtin_llfloorl (x);
}
