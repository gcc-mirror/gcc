/* { dg-do compile } */
/* { dg-options "-O3 -fassociative-math -fno-signed-zeros -fno-trapping-math -mavx512bf16 -mavx512vl" } */

/* vdpbf16ps ignores MXCSR: denormal inputs and results are flushed to
   zero regardless of the DAZ/FTZ bits.  Reassociation alone does not
   permit that, so without -funsafe-math-optimizations the dot product
   must not be expanded to vdpbf16ps.  */

float
dot_bf16 (const __bf16 *restrict a, const __bf16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler-not {\tvdpbf16ps\t} } } */
