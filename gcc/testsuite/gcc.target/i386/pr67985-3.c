/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -miamcu -mfpmath=sse -march=lakemont" } */

float
__attribute__((target("arch=haswell")))
foo (float x, float y)
{
  return x * y;
}

/* { dg-final { scan-assembler "mulss" } } */
/* { dg-final { scan-assembler "movd\[ \t\]%xmm\[0-7\], %eax" } } */
