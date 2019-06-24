/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "mulsd" 1 } } */
/* { dg-final { scan-assembler-not "movapd" } } */
/* { dg-final { scan-assembler-not "movsd" } } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));

__v2df
_mm_mul_sd (__v2df x, __v2df y)
{
  __v2df z = x;
  z[0] = x[0] * y[0];
  return z;
}
