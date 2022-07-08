/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d" } */
/* { dg-final { scan-assembler "mulw.d.w" } } */

/* This should be optimized to mulw.d.w for LA64.  */
__attribute__((noipa, noinline)) long
f(long a, long b)
{
  return (long)(int)a * (long)(int)b;
}
