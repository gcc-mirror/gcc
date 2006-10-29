/* { dg-do assemble } */

long testlf (float x)
{
  return __builtin_lceilf (x);
}
long testl (double x)
{
  return __builtin_lceil (x);
}
long testll (long double x)
{
  return __builtin_lceill (x);
}
long long testllf (float x)
{
  return __builtin_llceilf (x);
}
long long testll_ (double x)
{
  return __builtin_llceil (x);
}
long long testlll (long double x)
{
  return __builtin_llceill (x);
}
