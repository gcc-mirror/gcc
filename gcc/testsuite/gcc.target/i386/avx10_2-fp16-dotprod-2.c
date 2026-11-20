/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -mavx10.2 -mprefer-vector-width=256 -fdump-tree-vect-details" } */

/* An fp16 dot product accumulated in float is a widening dot product;
   with AVX10.2 the vectorizer should use vdpphps (sdot_prod optab).  The
   vectorized epilogue uses the next narrower form.  */

float
dot_fp16 (const _Float16 *restrict a, const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-tree-dump "dot_prod pattern recognized" "vect" } } */
/* { dg-final { scan-assembler {\tvdpphps\t[^\n]*%ymm[0-9]+, %ymm[0-9]+\n} } } */
/* { dg-final { scan-assembler {\tvdpphps\t[^\n]*%xmm[0-9]+, %xmm[0-9]+\n} } } */
