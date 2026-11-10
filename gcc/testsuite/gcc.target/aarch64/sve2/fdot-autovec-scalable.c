/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -march=armv9.4-a+sve2p1 -mautovec-preference=sve-only" } */

float
dot_half_scalable (const _Float16 *restrict a,
		   const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-assembler {\tfdot\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h} } } */
