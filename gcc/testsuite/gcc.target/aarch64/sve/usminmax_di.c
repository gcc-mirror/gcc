/* { dg-do compile } */
/* { dg-options "-O2 --param aarch64-autovec-preference=asimd-only" } */

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

#define FUNC(T, N, S)   \
void min_##S (T * __restrict__ a, T * __restrict__ b, T * __restrict__ c)  \
{									   \
  int i;								   \
  for (i = 0; i < N; i++)						   \
    c[i] = MIN (a[i], b[i]);						   \
}									   \
void max_##S (T * __restrict__ a, T * __restrict__ b, T * __restrict__ c)  \
{									   \
  int i;								   \
  for (i = 0; i < N; i++)						   \
    c[i] = MAX (a[i], b[i]);						   \
}									   \
void min_imm_##S (T * __restrict__ a, T * __restrict__ b, T * __restrict__ c) \
{									      \
  int i;								      \
  for (i = 0; i < N; i++)						      \
    c[i] = MIN (a[i], 5);						      \
}									      \
void max_imm_##S (T * __restrict__ a, T * __restrict__ b, T * __restrict__ c) \
{									      \
  int i;								      \
  for (i = 0; i < N; i++)						      \
    c[i] = MAX (a[i], 8);						      \
}

/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
FUNC (long long, 2, di)

/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumax\tz[0-9]+\.d, z[0-9]+\.d, #8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
FUNC (unsigned long long, 2, udi)

