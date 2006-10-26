/* { dg-do assemble } */

long testlf (float x)
{
  return __builtin_lrintf (x);
}
long testl (double x)
{
  return __builtin_lrint (x);
}
long testll (long double x)
{
  return __builtin_lrintl (x);
}
long long testllf (float x)
{
  return __builtin_llrintf (x);
}
long long testll_ (double x)
{
  return __builtin_llrint (x);
}
long long testlll (long double x)
{
  return __builtin_llrintl (x);
}
