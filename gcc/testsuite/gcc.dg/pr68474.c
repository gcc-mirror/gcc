/* { dg-options "-O -funsafe-math-optimizations" } */

long double
foo (long double d1, long double d2)
{
  return d1 || __builtin_significandl (d2);
}
