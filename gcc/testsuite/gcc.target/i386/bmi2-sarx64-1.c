/* { dg-do run { target { bmi2 && { ! ia32 } } } } */
/* { dg-options "-mbmi2 -O2 -dp" } */

#include "bmi2-check.h"

__attribute__((noinline))
long long
calc_sarx_u64 (long long a, int l)
{
  long long volatile res = a;
  int i;
  for (i = 0; i < l; ++i)
    res >>= 1;

  return res;
}

static void
bmi2_test ()
{
  unsigned i;
  long long src = 0xfce7ace0ce7ace0;
  long long res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_sarx_u64 (src, i + 1);
    res = src >> (i + 1);

    if (res != res_ref)
      abort();
  }
}
