/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "slli\\.d" } } */
/* { dg-final { scan-assembler-not "srli\\.d" } } */
/* { dg-final { scan-assembler-times "bytepick\\.d" 1 } } */

unsigned long
bytepick_d_n (unsigned long a, unsigned long b)
{
  return a >> 56 | b << 8;
}
