/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi-check.h"

/* To fool the compiler, so it does not generate blsi here. */
long long calc_blsi_u64 (long long src1, long long src2)
{
  return (-src1) & (src2);
}

static void
bmi_test()
{
  unsigned i;

  long long src = 0xfacec0ffeefacec0;
  long long res, res_ref;

  for (i=0; i<5; ++i) {
    src = (i + src) << i;

    res_ref = calc_blsi_u64 (src, src);
    res = __blsi_u64 (src);

    if (res != res_ref)
      abort();
  }
}
