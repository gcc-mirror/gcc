/* { dg-do run { target { bmi2 && { ! ia32 } } } } */
/* { dg-options "-mbmi2 -O2 -dp" } */

#include "bmi2-check.h"

__attribute__((noinline))
unsigned long long
calc_rorx_u64 (unsigned long long a, int l)
{
  unsigned long long volatile res = a;
  int i;
  for (i = 0; i < l; ++i)
    res = (res >> 1) | ((res&1)<< 63);

  return res;
}

#define SHIFT_VAL 0x1e

static void
bmi2_test ()
{
  unsigned i;
  unsigned long long src = 0xce7ace0ce7ace0;
  unsigned long long res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_rorx_u64 (src, SHIFT_VAL);
    res = (src >> SHIFT_VAL) | (src << (64 - SHIFT_VAL));

    if (res != res_ref)
      abort();
  }
}
