/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_cpu_supports_hw } */

#define NO_WARN_X86_INTRINSICS 1
#include <x86intrin.h>
#include "bmi-check.h"

int calc_tzcnt_u32 (int src)
{
  int i;
  int res = 0;

  while ( (res<32) && ((src&1) == 0)) {
    ++res;
    src >>= 1;
  }
  return res;
}

static void
bmi_test ()
{
  unsigned i;
  int src = 0xfacec0ff;
  int res, res_ref;

  for (i=0; i<5; ++i) {
    src = i + (src << i);

    res_ref = calc_tzcnt_u32 (src);
    res = __tzcnt_u32 (src);

    if (res != res_ref)
      abort();
  }
}
