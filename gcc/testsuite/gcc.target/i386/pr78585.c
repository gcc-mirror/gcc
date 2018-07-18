/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-Os -fno-ipa-cp -fschedule-insns -mavx" } */

#include "avx-check.h"

typedef unsigned int u32;
typedef unsigned long long u64;

u32 x0, x1, x2, x3, x4;
u64 x5, x6;

static u64 __attribute__ ((noinline, noclone))
foo (u64 x7)
{
  x6 = x2;
  x6 *= 5;
  x6--;
  return x0 + x5 + x1 + x7 + 1 + x3 + x4;
}

static void
__attribute__ ((noinline))
avx_test ()
{
  u64 x = foo (0);
  __builtin_printf ("%016llx", (unsigned long long) (x >> 0));
  __builtin_printf ("\n");
}
