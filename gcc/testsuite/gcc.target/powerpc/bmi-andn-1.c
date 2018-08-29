/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi-check.h"

long long calc_andn_u64 (long long src1,
			 long long src2,
			 long long dummy)
{
  return (~src1 + dummy) & (src2);
}

static void
bmi_test()
{
  unsigned i;

  long long src = 0xfacec0ffeefacec0;
  long long res, res_ref;

  for (i=0; i<5; ++i) {
    src = (i + src) << i;

    res_ref = calc_andn_u64 (src, src+i, 0);
    res = __andn_u64 (src, src+i);

    if (res != res_ref)
      abort();
  }
}
