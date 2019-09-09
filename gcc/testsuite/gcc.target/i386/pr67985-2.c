/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -miamcu -msse2 -mfpmath=sse -mtune=generic" } */

float
__attribute__((target("arch=lakemont")))
foo (float x, float y)
{
  return x * y;
}

/* { dg-final { scan-assembler-not "mulss" } } */
/* { dg-final { scan-assembler-not "movl\[ \t\].*, %eax" } } */
/* { dg-final { scan-assembler "call\[ \t\]_?__mulsf3" } } */
