/* { dg-do run } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fvect-cost-model=unlimited" } */

#include "tree-vect.h"

typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;
typedef __INT16_TYPE__ i16;
typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

#define N 259

static i32 in32[N];
static i64 in64[N];
static i16 out16[N];
static i32 out32[N];

__attribute__((noipa)) static void
clamp_s16_lo (i16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = x < -32768 ? -32768 : (x > 32767 ? 32767 : x);
    }
}

__attribute__((noipa)) static void
clamp_s16_hi (i16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = x > 32767 ? 32767 : (x < -32768 ? -32768 : x);
    }
}

__attribute__((noipa)) static void
clamp_s32_lo (i32 *__restrict out, const i64 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i64 x = in[i];
      out[i] = (x < -2147483647LL - 1
		? -2147483647LL - 1 : (x > 2147483647LL
				      ? 2147483647LL : x));
    }
}

__attribute__((noipa)) static void
clamp_s32_hi (i32 *__restrict out, const i64 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i64 x = in[i];
      out[i] = (x > 2147483647LL
		? 2147483647LL : (x < -2147483647LL - 1
				   ? -2147483647LL - 1 : x));
    }
}

static i16
ref_s16 (i32 x)
{
  return x < -32768 ? -32768 : (x > 32767 ? 32767 : x);
}

static i32
ref_s32 (i64 x)
{
  return (x < -2147483647LL - 1
	  ? -2147483647LL - 1 : (x > 2147483647LL ? 2147483647LL : x));
}

static void
check_s16 (void)
{
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (out16[i] != ref_s16 (in32[i]))
      __builtin_abort ();
}

static void
check_s32 (void)
{
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (out32[i] != ref_s32 (in64[i]))
      __builtin_abort ();
}

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; ++i)
    {
      in32[i] = (i32) ((u32) i * 2654435761U + 1013904223U);
      in64[i] = (i64) ((u64) i * 11400714819323198485ULL
			     + 13787848793156543929ULL);
    }

  in32[0] = -32769;
  in32[1] = -32768;
  in32[2] = 32767;
  in32[3] = 32768;
  in64[0] = -2147483647LL - 2;
  in64[1] = -2147483647LL - 1;
  in64[2] = 2147483647LL;
  in64[3] = 2147483648LL;

  clamp_s16_lo (out16, in32, N);
  check_s16 ();
  clamp_s16_hi (out16, in32, N);
  check_s16 ();
  clamp_s32_lo (out32, in64, N);
  check_s32 ();
  clamp_s32_hi (out32, in64, N);
  check_s32 ();

  return 0;
}
