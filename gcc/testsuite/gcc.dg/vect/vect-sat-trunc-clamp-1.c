/* { dg-do run } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fvect-cost-model=unlimited" } */

#include "tree-vect.h"

typedef __UINT16_TYPE__ u16;
typedef __UINT32_TYPE__ u32;
typedef __UINT64_TYPE__ u64;
typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

#define N 259

static i32 in32[N];
static i64 in64[N];
static u16 out16[N];
static u32 out32[N];

__attribute__((noipa)) static void
clamp_u16_lo (u16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = x < 0 ? 0 : (x > 65535 ? 65535 : x);
    }
}

__attribute__((noipa)) static void
clamp_u16_hi (u16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = x > 65535 ? 65535 : (x < 0 ? 0 : x);
    }
}

__attribute__((noipa)) static void
clamp_u32_lo (u32 *__restrict out, const i64 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i64 x = in[i];
      out[i] = x < 0 ? 0 : (x > 4294967295LL ? 4294967295LL : x);
    }
}

__attribute__((noipa)) static void
clamp_u32_hi (u32 *__restrict out, const i64 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i64 x = in[i];
      out[i] = x > 4294967295LL ? 4294967295LL : (x < 0 ? 0 : x);
    }
}

static u16
ref_u16 (i32 x)
{
  return x < 0 ? 0 : (x > 65535 ? 65535 : x);
}

static u32
ref_u32 (i64 x)
{
  return x < 0 ? 0 : (x > 4294967295LL ? 4294967295LL : x);
}

static void
check_u16 (void)
{
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (out16[i] != ref_u16 (in32[i]))
      __builtin_abort ();
}

static void
check_u32 (void)
{
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (out32[i] != ref_u32 (in64[i]))
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

  in32[0] = -1;
  in32[1] = 0;
  in32[2] = 65535;
  in32[3] = 65536;
  in64[0] = -1;
  in64[1] = 0;
  in64[2] = 4294967295LL;
  in64[3] = 4294967296LL;

  clamp_u16_lo (out16, in32, N);
  check_u16 ();
  clamp_u16_hi (out16, in32, N);
  check_u16 ();
  clamp_u32_lo (out32, in64, N);
  check_u32 ();
  clamp_u32_hi (out32, in64, N);
  check_u32 ();

  return 0;
}
