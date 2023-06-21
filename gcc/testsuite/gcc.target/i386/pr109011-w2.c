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
#include "pr109011-w1.c"

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
popcntw_scalar (unsigned short *p, unsigned short *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcount (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzw_scalar (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clz (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ffsw_scalar (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffs (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzw0_scalar (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clz (q[i]) : 16;
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ctzw0_scalar (unsigned short *p, unsigned short* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctz (q[i]) : 16;
}

void
test_256 ()
{
  unsigned short src[2048];
  unsigned short res[2048];
  unsigned short exp[2048];
  for (int i = 0; i != 2048; i++)
    {
      src[i] = i * i - 1;
      res[i] = 0;
      exp[i] = 1;
    }

  popcntw (&res[0], &src[0]);
  popcntw_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 2) != 0)
    __builtin_abort ();

  clzw (&res[0], &src[0]);
  clzw_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 2) != 0)
    __builtin_abort ();

  ffsw (&res[0], &src[0]);
  ffsw_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 2) != 0)
    __builtin_abort ();

  clzw0 (&res[0], &src[0]);
  clzw0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 2) != 0)
    __builtin_abort ();

  ctzw0 (&res[0], &src[0]);
  ctzw0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048 * 2) != 0)
    __builtin_abort ();
}

void
test_128 ()
{}
