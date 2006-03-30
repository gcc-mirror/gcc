/* { dg-do compile } */
/* { dg-options "-msse2 -mfpmath=sse -msselibm" } */
/* { dg-require-effective-target ilp32 } */

float sinf(float);

float foo(float x)
{
  return sinf(x);
}

/* { dg-final { scan-assembler "__libm_sse2_sinf" } } */
