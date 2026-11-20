/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -mavx512bf16 -mavx512vl -mprefer-vector-width=256 -fdump-tree-vect-details" } */

/* A bf16 dot product accumulated in float is a widening dot product;
   with AVX512BF16 the vectorizer should use vdpbf16ps (sdot_prod optab).  */

float
dot_bf16 (const __bf16 *restrict a, const __bf16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-tree-dump "dot_prod pattern recognized" "vect" } } */
/* { dg-final { scan-assembler {\tvdpbf16ps\t[^\n]*%ymm[0-9]+, %ymm[0-9]+\n} } } */
