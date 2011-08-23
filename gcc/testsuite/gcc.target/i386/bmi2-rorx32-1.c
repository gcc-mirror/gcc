/* { dg-do run { target { bmi2 } } } */
/* { dg-options "-mbmi2 -O2 -dp" } */

#include "bmi2-check.h"

__attribute__((noinline))
unsigned
calc_rorx_u32 (unsigned a, int l)
{
  unsigned volatile res = a;
  int i;
  for (i = 0; i < l; ++i)
    res = (res >> 1) | ((res & 1) << 31);

  return res;
}

#define SHIFT_VAL 0x0e

static void
bmi2_test ()
{
  unsigned i;
  unsigned src = 0xce7ace0;
  unsigned res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_rorx_u32 (src, SHIFT_VAL);
    res = (src >> SHIFT_VAL) | (src << (32 - SHIFT_VAL));

    if (res != res_ref)
      abort();
  }
}
