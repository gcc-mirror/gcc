/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -O2" } */
/* { dg-final { scan-assembler "alsl\\.wu" } } */

unsigned long
test (unsigned int a, unsigned int b)
{
  return (a << 2) + b;
}
