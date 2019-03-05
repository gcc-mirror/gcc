/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -fdump-tree-vect-details --save-temps" } */

void
xorsign_half (_Float16 * restrict a, _Float16 * restrict b,
	      _Float16 * restrict r, int n)
{
  for (int i = 0; i < n; i++)
    {
      r[i] = a[i] * __builtin_copysignf16 (1.0f16, b[i]);
    }
}

void
xorsign_float (float *restrict a, float *restrict b, float *restrict r, int n)
{
  for (int i = 0; i < n; i++)
    {
      r[i] = a[i] * __builtin_copysignf (1.0f, b[i]);
    }
}

void
xorsign_double (double *restrict a, double *restrict b, double *restrict r,
		int n)
{
  for (int i = 0; i < n; i++)
    {
      r[i] = a[i] * __builtin_copysign (1.0d, b[i]);
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 3 "vect" } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, z[0-9]+\.h, #0x8000\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x80000000\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x8000000000000000\n} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
