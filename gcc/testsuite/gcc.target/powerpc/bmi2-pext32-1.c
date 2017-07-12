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
unsigned
calc_pext_u32 (unsigned a, unsigned mask)
{
  unsigned res = 0;
  int i, k = 0;

  for (i = 0; i < 32; ++i)
    if (mask & (1 << i)) {
      res |= ((a & (1 << i)) >> i) << k;
      ++k;
    }

  return res;
}

static void
bmi2_test ()
{
  unsigned i;
  unsigned src = 0xce7acc;
  unsigned res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_pext_u32 (src, ~(i * 3));
    res = _pext_u32 (src, ~(i * 3));

    if (res != res_ref)
      abort();
  }
}
