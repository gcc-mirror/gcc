/* { dg-do run { target { lzcnt } } } */
/* { dg-options "-O2 -mlzcnt -fno-inline" } */

#include <x86intrin.h>

#include "lzcnt-check.h"

short calc_lzcnt_u16 (short src)
{
  int i;
  short res = 0;

  while ((res < 16) && (((src >> (15 - res)) & 1) == 0))
    ++res;

  return res;
}

static void
lzcnt_test ()
{
  unsigned i;
  short src = 0x7ace;
  short res, res_ref;

  for (i=0; i<5; ++i) {
    src = src >> i;

    res_ref = calc_lzcnt_u16 (src);
    res = __lzcnt16 (src);

    if (res != res_ref)
      abort();
  }
}
