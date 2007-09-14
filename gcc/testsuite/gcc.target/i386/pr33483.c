/* { dg-do compile } */
/* { dg-options "-O2" } */

long double f1 (long double x)
{
  return __builtin_fmodl (x, x);
}

long double f2 (long double x)
{
  return __builtin_remainderl (x, x);
}
