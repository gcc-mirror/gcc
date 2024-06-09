/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long long foo(unsigned long long x)
{
  return (x >> 1) | (x << 63);
}

/* { dg-final { scan-assembler "asr.f\\s+0,r0" } } */
/* { dg-final { scan-assembler "rrc.f\\s+r1,r1" } } */
/* { dg-final { scan-assembler "rrc\\s+r0,r0" } } */
