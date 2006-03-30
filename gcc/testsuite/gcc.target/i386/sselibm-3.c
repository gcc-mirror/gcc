/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -msselibm" } */
/* { dg-require-effective-target ilp32 } */

float sinf(float);
float (*mysin)(float) = sinf;

float f1(float x)
{
  return sinf(x);
}

float f2(float x)
{
  /* Verify we do not expand the following call to __libm_sse2_sinf.  */
  return (*mysin)(x);
}

/* { dg-final { scan-assembler-times "__libm_sse2_sinf" 1 } } */
