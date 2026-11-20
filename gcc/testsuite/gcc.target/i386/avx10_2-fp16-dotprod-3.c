/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -mavx512fp16 -mavx512vl -mno-avx10.2" } */

/* The pattern is recognized, but without AVX10.2 there is no optab, so
   no vdpphps may be emitted.  */

float
dot_fp16 (const _Float16 *restrict a, const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler-not {\tvdpphps\t} } } */
