/* { dg-do run } */
/* { dg-options "-O3 -mbmi -mlzcnt -mavx512vl -mavx512cd -mavx512bitalg -mavx512vpopcntdq -mprefer-vector-width=256" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512cd } */
/* { dg-require-effective-target avx512bitalg } */
/* { dg-require-effective-target avx512vpopcntdq } */

#define AVX512F
#define AVX512VL
#define AVX512CD
#define AVX512BITALG
#define AVX512VPOPCNTDQ

#include "avx512f-helper.h"
#include "pr109011-dq1.c"

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
popcntd_scalar (unsigned int *p, unsigned int *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcountll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzd_scalar (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clzll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ffsd_scalar (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffsll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzd0_scalar (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clzll (q[i]) : 32;
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ctzd0_scalar (unsigned int *p, unsigned int* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctzll (q[i]) : 32;
}

void
test_256 ()
{
  unsigned int src[2048];
  unsigned int res[2048];
  unsigned int exp[2048];
  for (int i = 0; i != 2048; i++)
    {
      src[i] = i * i - 1;
      res[i] = 0;
      exp[i] = 1;
    }

  popcntd (&res[0], &src[0]);
  popcntd_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 4) != 0)
    __builtin_abort ();

  clzd (&res[0], &src[0]);
  clzd_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (&res[0], &exp[0], 2048 * 4) != 0)
    __builtin_abort ();

  ffsd (&res[0], &src[0]);
  ffsd_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 4) != 0)
    __builtin_abort ();

  clzd0 (&res[0], &src[0]);
  clzd0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 4) != 0)
    __builtin_abort ();

  ctzd0 (&res[0], &src[0]);
  ctzd0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 4) != 0)
    __builtin_abort ();
}

void
test_128 ()
{}
