/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -fdump-tree-vect-details --save-temps" } */

void
copysign_half (_Float16 * restrict a, _Float16 * restrict b,
	       _Float16 * restrict r, int n)
{
  for (int i = 0; i < n; i++)
    {
      r[i] = __builtin_copysignf16 (a[i], b[i]);
    }
}

void
copysign_float (float *restrict a, float *restrict b, float *restrict r,
		int n)
{
  for (int i = 0; i < n; i++)
    {
      r[i] = __builtin_copysignf (a[i], b[i]);
    }
}

void
copysign_double (double *restrict a, double *restrict b, double *restrict r,
		 int n)
{
  for (int i = 0; i < n; i++)
    {
      r[i] = __builtin_copysign (a[i], b[i]);
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 3 "vect" } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, z[0-9]+\.h, #0x8000\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, z[0-9]+\.h, #0x7fff\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x80000000\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s, #0x7fffffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x8000000000000000\n} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d, #0x7fffffffffffffff\n} 1 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
