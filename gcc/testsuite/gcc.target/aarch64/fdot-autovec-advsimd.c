/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -march=armv8.2-a+fp16+f16f32dot -mautovec-preference=asimd-only -fdump-tree-vect-details" } */

float
dot_half_advsimd (const _Float16 *restrict a,
		  const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler {\tfdot\tv[0-9]+\.4s, v[0-9]+\.8h, v[0-9]+\.8h} } } */
/* { dg-final { scan-assembler {\tfdot\tv[0-9]+\.2s, v[0-9]+\.4h, v[0-9]+\.4h} } } */
/* { dg-final { scan-tree-dump "dot_prod pattern recognized" "vect" } } */
