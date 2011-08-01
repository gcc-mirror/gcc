/* { dg-do run { target { lzcnt } } } */
/* { dg-options "-O2 -mlzcnt -fno-inline" } */

#include <x86intrin.h>

#include "lzcnt-check.h"

int calc_lzcnt_u32 (int src)
{
  int i;
  int res = 0;

  while ((res < 32) && (((src >> (31 - res)) & 1) == 0))
    ++res;

  return res;
}

static void
lzcnt_test ()
{
  unsigned i;
  int src = 0xce7ace0;
  int res, res_ref;

  for (i=0; i<5; ++i) {
    src = src >> i;

    res_ref = calc_lzcnt_u32 (src);
    res = __lzcnt32 (src);

    if (res != res_ref)
      abort();
  }
}
