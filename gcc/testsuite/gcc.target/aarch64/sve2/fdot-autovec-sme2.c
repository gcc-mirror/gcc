/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -march=armv9.2-a+sme2 -mautovec-preference=sve-only" } */

__arm_streaming float
dot_half_sme2 (const _Float16 *restrict a,
	       const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler {\tfdot\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h} } } */
