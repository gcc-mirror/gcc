/* { dg-do run } */
/* { dg-require-effective-target f16c } */
/* { dg-options "-O2 -mf16c" } */

#include "f16c-check.h"

static void
f16c_test (void)
{
  union256 val;
  union128i_w res; 
  short exp[8];

  val.a[0] = 1;
  val.a[1] = 2;
  val.a[2] = 4;
  val.a[3] = 8;
  val.a[4] = -1;
  val.a[5] = -2;
  val.a[6] = -4;
  val.a[7] = -8;

  exp[0] = 0x3c00;
  exp[1] = 0x4000;
  exp[2] = 0x4400;
  exp[3] = 0x4800;
  exp[4] = 0xbc00;
  exp[5] = 0xc000;
  exp[6] = 0xc400;
  exp[7] = 0xc800;

  res.x = _mm256_cvtps_ph (val.x, 0);

  if (check_union128i_w (res, exp))
    abort ();
}
