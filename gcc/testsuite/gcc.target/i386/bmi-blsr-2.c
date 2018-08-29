/* { dg-do run { target bmi } } */
/* { dg-options "-O2 -mbmi -fno-inline" } */

#include <x86intrin.h>

#include "bmi-check.h"

int calc_blsr_u32 (int src1, int src2)
{
  return (src1-1) & (src2);
}

static void
bmi_test ()
{
  unsigned i;
  int src = 0xfacec0ff;
  int res, res_ref;

  for (i=0; i<5; ++i) {
    src = i + src << i;

    res_ref = calc_blsr_u32 (src, src);
    res = __blsr_u32 (src);

    if (res != res_ref)
      abort();
  }
}
