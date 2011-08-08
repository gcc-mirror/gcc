/* { dg-do run { target { lzcnt && { ! ia32 } } } } */
/* { dg-options "-O2 -mlzcnt -fno-inline" } */

#include <x86intrin.h>

#include "lzcnt-check.h"

long long calc_lzcnt_u64 (long long src)
{
  int i;
  int res = 0;

  while ((res < 64) && (((src >> (63 - res)) & 1) == 0))
    ++res;

  return res;
}

static void
lzcnt_test ()
{
  unsigned i;
  long long src = 0xce7ace0ce7ace0;
  long long res, res_ref;

  for (i=0; i<5; ++i) {
    src = src >> i;

    res_ref = calc_lzcnt_u64 (src);
    res = __lzcnt64 (src);

    if (res != res_ref)
      abort();
  }
}
