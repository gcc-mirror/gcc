/* { dg-do compile } */
/* { dg-options "-O2" } */

long long foo(long long x, long long y)
{
  return x + y;
}

/* { dg-final { scan-assembler "add.f\\s+r0,r0,r2" } } */
/* { dg-final { scan-assembler "adc\\s+r1,r1,r3" } } */
