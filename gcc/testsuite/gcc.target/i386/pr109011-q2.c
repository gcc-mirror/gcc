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
#include "pr109011-q1.c"

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
popcntq_scalar (unsigned long long *p, unsigned long long *q)
{
  for (unsigned long long i = 0; i < 2048; ++i)
    p[i] = __builtin_popcountll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzq_scalar (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned long long i = 0; i < 2048; ++i)
    p[i] = __builtin_clzll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ffsq_scalar (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned long long i = 0; i < 2048; ++i)
    p[i] = __builtin_ffsll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ctzq_scalar (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned long long i = 0; i < 2048; ++i)
    p[i] = __builtin_ctzll (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzq0_scalar (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned long long i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clzll (q[i]) : 64;
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ctzq0_scalar (unsigned long long *p, unsigned long long* __restrict q)
{
  for (unsigned long long i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctzll (q[i]) : 64;
}

void
test_256 ()
{
  unsigned long long src[2048];
  unsigned long long res[2048];
  unsigned long long exp[2048];
  for (unsigned long long i = 0; i != 2048ULL; i++)
    {
      src[i] = i * i - 1ULL;
      res[i] = 0;
      exp[i] = 1;
    }

  popcntq (&res[0], &src[0]);
  popcntq_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 8) != 0)
    __builtin_abort ();

  clzq (&res[0], &src[0]);
  clzq_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 8) != 0)
    __builtin_abort ();

  ffsq (&res[0], &src[0]);
  ffsq_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 8) != 0)
    __builtin_abort ();

  ctzq (&res[0], &src[0]);
  ctzq_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 8) != 0)
    __builtin_abort ();

  clzq0 (&res[0], &src[0]);
  clzq0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 8) != 0)
    __builtin_abort ();

  ctzq0 (&res[0], &src[0]);
  ctzq0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 8) != 0)
    __builtin_abort ();
}

void
test_128 ()
{}
