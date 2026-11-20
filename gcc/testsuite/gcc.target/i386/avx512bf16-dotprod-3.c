/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -mavx512vl -mno-avx512bf16" } */

/* The pattern is recognized, but without AVX512BF16 there is no optab, so
   no vdpbf16ps may be emitted.  */

float
dot_bf16 (const __bf16 *restrict a, const __bf16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler-not {\tvdpbf16ps\t} } } */
