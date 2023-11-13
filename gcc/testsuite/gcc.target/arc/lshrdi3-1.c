/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long long foo(unsigned long long x)
{
  return x >> 1;
}

/* { dg-final { scan-assembler "lsr.f\\s+r1,r1" } } */
/* { dg-final { scan-assembler "rrc\\s+r0,r0" } } */
