/* { dg-do assemble } */

long testlf (float x)
{
  return __builtin_lroundf (x);
}
long testl (double x)
{
  return __builtin_lround (x);
}
long testll (long double x)
{
  return __builtin_lroundl (x);
}
long long testllf (float x)
{
  return __builtin_llroundf (x);
}
long long testll_ (double x)
{
  return __builtin_llround (x);
}
long long testlll (long double x)
{
  return __builtin_llroundl (x);
}
