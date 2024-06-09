/* { dg-do compile } */
/* { dg-options "-O2" } */

long long foo(long long x)
{
  return x >> 1;
}

/* { dg-final { scan-assembler "asr.f\\s+r1,r1" } } */
/* { dg-final { scan-assembler "rrc\\s+r0,r0" } } */
