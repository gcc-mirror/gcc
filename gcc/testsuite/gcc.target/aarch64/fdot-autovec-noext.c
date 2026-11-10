/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -march=armv8.2-a+fp16 -mautovec-preference=asimd-only" } */

float
dot_half_noext (const _Float16 *restrict a,
		const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler-not {\tfdot\t} } } */
