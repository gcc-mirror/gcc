/* { dg-do run } */
/* { dg-require-effective-target f16c } */
/* { dg-options "-O2 -mf16c" } */

#include "f16c-check.h"

static void
f16c_test (void)
{
  float val = -2;
  unsigned short exp = 0xc000;
  unsigned short res;

  res = _cvtss_sh (val, 0);

  if (res != exp)
    abort ();
}
