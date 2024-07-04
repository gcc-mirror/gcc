/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long long foo(unsigned long long x)
{
  return (x << 1) | (x >> 63);
}

/* { dg-final { scan-assembler "add.f\\s+r0,r0,r0" } } */
/* { dg-final { scan-assembler "adc.f\\s+r1,r1,r1" } } */
/* { dg-final { scan-assembler "add.cs\\s+r0,r0,1" } } */
