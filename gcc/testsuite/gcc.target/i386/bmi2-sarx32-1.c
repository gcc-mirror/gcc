/* { dg-do run { target { bmi2 } } } */
/* { dg-options "-mbmi2 -O2 -dp" } */

#include "bmi2-check.h"

__attribute__((noinline))
int
calc_sarx_u32 (int a, int l)
{
  int volatile res = a;
  int i;
  for (i = 0; i < l; ++i)
    res >>= 1;

  return res;
}

static void
bmi2_test ()
{
  unsigned i;
  int src = 0xfce7ace0;
  int res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_sarx_u32 (src, i + 1);
    res = src >> (i + 1);

    if (res != res_ref)
      abort();
  }
}
