/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times "psrld" 1 } } */

__bf16
foo (float a)
{
  return a;
}
