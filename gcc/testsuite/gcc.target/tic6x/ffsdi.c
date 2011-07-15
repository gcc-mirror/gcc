/* { dg-do compile } */
/* { dg-require-effective-target ti_c64xp } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "call" } } */

long long foo (long long x)
{
  return __builtin_ffsll (x);
}

long long bar (long long x)
{
  return __builtin_clzll (x);
}

long long baz (long long x)
{
  return __builtin_ctzll (x);
}
