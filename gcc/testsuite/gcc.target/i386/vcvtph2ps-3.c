/* { dg-do run } */
/* { dg-require-effective-target f16c } */
/* { dg-options "-O2 -mf16c" } */

#include "f16c-check.h"

static void
f16c_test (void)
{
  unsigned short val = 0xc000;
  float exp = -2;
  float res;

  res = _cvtsh_ss (val);

  if (res != exp)
    abort ();
}
