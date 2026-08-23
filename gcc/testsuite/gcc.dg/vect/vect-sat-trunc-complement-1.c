/* { dg-do run } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fvect-cost-model=unlimited" } */

#include "tree-vect.h"

typedef __UINT16_TYPE__ u16;
typedef __UINT32_TYPE__ u32;
typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

#define N 96

static i32 in32[N];
static i64 in64[N];
static u16 out16[N];
static u32 out32[N];

__attribute__((noipa)) static void
clip_u16 (u16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = (x & ~65535) ? (~x) >> 31 : x;
    }
}

__attribute__((noipa)) static void
clip_u32 (u32 *__restrict out, const i64 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i64 x = in[i];
      out[i] = (x & ~(i64) 0xffffffff) ? (~x) >> 63 : x;
    }
}

int
main (void)
{
  static const i32 values32[6] =
    { (-2147483647 - 1), -1, 0, 65535, 65536, 2147483647 };
  static const u16 expected16[6] =
    { 0, 0, 0, 65535, 65535, 65535 };
  static const i64 values64[6] =
    { (-9223372036854775807LL - 1), -1, 0, 4294967295LL,
      4294967296LL, 9223372036854775807LL };
  static const u32 expected32[6] =
    { 0, 0, 0, (u32) -1, (u32) -1, (u32) -1 };

  check_vect ();

  for (int i = 0; i < N; ++i)
    {
      in32[i] = values32[i % 6];
      in64[i] = values64[i % 6];
    }

  clip_u16 (out16, in32, N);
  clip_u32 (out32, in64, N);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (out16[i] != expected16[i % 6]
	|| out32[i] != expected32[i % 6])
      __builtin_abort ();

  return 0;
}
