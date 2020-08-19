/* { dg-do compile } */
/* { dg-options "-Ofast -mno-avx512f -mfma" } */

extern float sqrtf (float);

void
rsqrt (float* restrict r, float* restrict a)
{
  for (int i = 0; i < 64; i++)
    r[i] = sqrtf(a[i]);
}

/* { dg-final { scan-assembler "\tvfmadd\[123\]+ps" } } */
