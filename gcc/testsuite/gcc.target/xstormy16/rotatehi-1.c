/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned short foo(unsigned short x)
{
  return (x << 1) | (x >> 15);
}

/* { dg-final { scan-assembler "shl r2,#1" } } */
/* { dg-final { scan-assembler "adc r2,#0" } } */
