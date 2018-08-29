/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi2-check.h"

__attribute__((noinline))
unsigned long long
calc_bzhi_u64 (unsigned long long a, int l)
{
  unsigned long long res = a;
  int i;
  for (i = 0; i < 64 - l; ++i)
    res &= ~(1LL << (63 - i));

  return res;
}

static void
bmi2_test ()
{
  unsigned i;
  unsigned long long src = 0xce7ace0ce7ace0ff;
  unsigned long long res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_bzhi_u64 (src, i * 2);
    res = _bzhi_u64 (src, i * 2);

    if (res != res_ref)
      abort();
  }
}
