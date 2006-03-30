/* { dg-do compile } */
/* { dg-options "-msse2 -mfpmath=sse" } */
/* { dg-require-effective-target ilp32 } */

float sinf(float);

float foo(float x)
{
  return sinf(x);
}

/* { dg-final { scan-assembler-not "__libm_sse2_sinf" } } */
