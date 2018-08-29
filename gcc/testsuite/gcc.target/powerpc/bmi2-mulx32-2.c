/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi2-check.h"

__attribute__((noinline))
unsigned long long
calc_mul_u32 (unsigned volatile a, unsigned b)
{
  unsigned long long res = 0;
  int i;
  for (i = 0; i < b; ++i)
    res += a;

  return res;
}

__attribute__((noinline))
unsigned calc_mulx_u32 (unsigned x, unsigned y, unsigned *res_h)
{
  return (unsigned) _mulx_u32 (x, y, res_h);
}

static void
bmi2_test ()
{
  unsigned i;
  unsigned a = 0xce7ace0;
  unsigned b = 0xfacefff;
  unsigned res_l, res_h;
  unsigned long long res, res_ref;

  for (i = 0; i < 5; ++i) {
    a = a * (i + 1);
    b = b / (i + 1);

    res_ref = calc_mul_u32 (a, b);
    res_l = calc_mulx_u32 (a, b, &res_h);

    res = ((unsigned long long) res_h << 32) | res_l;

    if (res != res_ref)
      abort();
  }
}
