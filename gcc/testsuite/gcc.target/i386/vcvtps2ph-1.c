/* { dg-do run } */
/* { dg-require-effective-target f16c } */
/* { dg-options "-O2 -mf16c" } */

#include "f16c-check.h"

static void
f16c_test (void)
{
  union128 val;
  union128i_w res; 
  short exp[8];

  val.a[0] = 1;
  val.a[1] = -2;
  val.a[2] = -1;
  val.a[3] = 2;

  exp[0] = 0x3c00;
  exp[1] = 0xc000;
  exp[2] = 0xbc00;
  exp[3] = 0x4000;
  exp[4] = 0;
  exp[5] = 0;
  exp[6] = 0;
  exp[7] = 0;

  res.x = _mm_cvtps_ph (val.x, 0);

  if (check_union128i_w (res, exp))
    abort ();
}
