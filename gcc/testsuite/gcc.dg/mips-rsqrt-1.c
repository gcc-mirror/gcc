/* { dg-do compile { target "mips*-*-*" } } */
/* { dg-options "-O2 -ffast-math -mips4" } */
/* { dg-final { scan-assembler "rsqrt.d" } } */
/* { dg-final { scan-assembler "rsqrt.s" } } */

#if (__mips != 4 && __mips != 64) || __mips_soft_float
asm ("# rsqrt.d rsqrt.s");
#else
extern double sqrt(double);
extern float sqrtf(float);

double foo(double x)
{
  return 1.0/sqrt(x);
}

float bar(float x)
{
  return 1.0f/sqrtf(x);
}
#endif
