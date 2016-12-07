/* { dg-do run } */
/* { dg-options "-O2 -fno-guess-branch-probability -fschedule-insns -fno-tree-ter -mavx512f --param=max-pending-list-length=512" } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

typedef unsigned char u8;
typedef unsigned char v64u8 __attribute__ ((vector_size (64)));
typedef unsigned short u16;
typedef unsigned short v64u16 __attribute__ ((vector_size (64)));
typedef unsigned int u32;
typedef unsigned int v64u32 __attribute__ ((vector_size (64)));
typedef unsigned long long u64;
typedef unsigned long long v64u64 __attribute__ ((vector_size (64)));
typedef unsigned __int128 u128;
typedef unsigned __int128 v64u128 __attribute__ ((vector_size (64)));

v64u128 __attribute__ ((noinline, noclone))
foo(u8 x1, u16 x2, u32 x3, u64 x4, v64u8 x5, v64u16 x6, v64u32 x7, v64u64 x8, v64u128 x9)
{
  u8 *p = &x1;
  x9[0] -= *p;
  x5 %= (v64u8){ 1, -x4, 0, x3, x5[9], x7[1], 4, x6[1], 13 << 4} | 1;
  x5[1] = x5[0];
  x8 %= (v64u64){1, x1} | 1;
  x9 /= x9 | 1;
  x5 -= (v64u8){0, 0, 3, 0, 0, 0, 0, x4, x9[0], 0, 1};
  return x1 + x2 + x3 + x4 + (v64u128) x5 + (v64u128) x6 + (v64u128) x7 +
    (v64u128) x8 + x9;
}

static void
avx512f_test (void)
{
  v64u128 x = foo(1, 0, 0, 0, (v64u8){}, (v64u16){}, (v64u32){}, (v64u64){}, (v64u128){});

  if ((u64)(x[0] >> 64) != 0x0000000000ff00ff)
    __builtin_abort();
  if ((u64)(x[0] >>  0) != 0x0000000000fd0002)
    __builtin_abort();

  if (x[1] != 1)
    __builtin_abort();
  if (x[2] != 1)
    __builtin_abort();
  if (x[3] != 1)
    __builtin_abort();
}
