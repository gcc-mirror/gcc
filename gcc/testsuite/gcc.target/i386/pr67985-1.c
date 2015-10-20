/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -miamcu -msse2 -mfpmath=sse -mtune=generic" } */

float
foo (float x, float y)
{
  return x * y;
}

/* { dg-final { scan-assembler "mulss" } } */
/* { dg-final { scan-assembler "movd\[ \t\]%xmm\[0-7\], %eax" } } */
