/* { dg-do run { target { bmi } } } */
/* { dg-options "-O2 -mbmi -fno-inline" } */

#include <x86intrin.h>

#include "bmi-check.h"

long long calc_andn_u32 (int src1, int src2, int dummy)
{
  return (~src1+dummy) & (src2);
}

static void
bmi_test()
{
  unsigned i;

  int src = 0xfacec0ff;
  int res, res_ref;

  for (i=0; i<5; ++i) {
    src = i + src << i;

    res_ref = calc_andn_u32 (src, src+i, 0);
    res = __andn_u32 (src, src+i);

    if (res != res_ref)
      abort();
  }
}
