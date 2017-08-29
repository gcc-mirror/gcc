/* { dg-do run { target bmi2 } } */
/* { dg-options "-mbmi2 -O2" } */

#include <x86intrin.h>
#include "bmi2-check.h"

__attribute__((noinline))
unsigned
calc_bzhi_u32 (unsigned a, int l)
{
  unsigned res = a;
  int i;
  for (i = 0; i < 32 - l; ++i)
    res &= ~(1 << (31 - i));

  return res;
}

static void
bmi2_test ()
{
  unsigned i;
  unsigned src = 0xce7ace0f;
  unsigned res, res_ref;

  for (i = 0; i < 5; ++i) {
    src = src * (i + 1);

    res_ref = calc_bzhi_u32 (src, i * 2);
    res = _bzhi_u32 (src, i * 2);

    if (res != res_ref)
      abort();
  }
}
