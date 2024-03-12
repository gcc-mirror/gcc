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
#include "pr109011-b1.c"

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
popcntb_scalar (unsigned char *p, unsigned char *q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_popcount (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzb_scalar (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_clz (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ffsb_scalar (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = __builtin_ffs (q[i]);
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
clzb0_scalar (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clz (q[i]) : 8;
}

void
__attribute__((noipa, optimize ("no-tree-vectorize")))
ctzb0_scalar (unsigned char *p, unsigned char* __restrict q)
{
  for (unsigned int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctz (q[i]) : 8;
}

void
test_256 ()
{
  unsigned char src[2048];
  unsigned char res[2048];
  unsigned char exp[2048];
  for (int i = 0; i != 2048; i++)
    {
      src[i] = i * i - 1;
      res[i] = 0;
      exp[i] = 1;
    }

  popcntb (&res[0], &src[0]);
  popcntb_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048) != 0)
    __builtin_abort ();

  clzb (&res[0], &src[0]);
  clzb_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048) != 0)
    __builtin_abort ();

  ffsb (&res[0], &src[0]);
  ffsb_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048) != 0)
    __builtin_abort ();

  clzb0 (&res[0], &src[0]);
  clzb0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048) != 0)
    __builtin_abort ();

  ctzb0 (&res[0], &src[0]);
  ctzb0_scalar (&exp[0], &src[0]);

  if (__builtin_memcmp (res, exp, 2048) != 0)
    __builtin_abort ();
}

void
test_128 ()
{}
