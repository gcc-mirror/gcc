/* { dg-do run } */
/* { dg-options "-O3 -mcpu=power7" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi2-check.h"

__attribute__((noinline))
unsigned long long
calc_pext_u64 (unsigned long long a, unsigned long long mask)
{
  unsigned long long res = 0;
  int i, k = 0;

  for (i = 0; i < 64; ++i)
    if (mask & (1LL << i)) {
      res |= ((a & (1LL << i)) >> i) << k;
      ++k;
    }

  return res;
}

static void
bmi2_test ()
{
  unsigned long long i;
  unsigned long long src = 0xce7acce7acce7ac;
  unsigned long long res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_pext_u64 (src, ~(i * 3));
    res = _pext_u64 (src, ~(i * 3));

    if (res != res_ref)
      abort();
  }
}
