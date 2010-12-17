/* { dg-do run } */
/* { dg-require-effective-target f16c } */
/* { dg-options "-O2 -mf16c" } */

#include "f16c-check.h"

static void
f16c_test (void)
{
  union256 res;
  union128i_w val; 
  float exp[8];

  exp[0] = 1;
  exp[1] = 2;
  exp[2] = 4;
  exp[3] = 8;
  exp[4] = -1;
  exp[5] = -2;
  exp[6] = -4;
  exp[7] = -8;

  val.a[0] = 0x3c00;
  val.a[1] = 0x4000;
  val.a[2] = 0x4400;
  val.a[3] = 0x4800;
  val.a[4] = 0xbc00;
  val.a[5] = 0xc000;
  val.a[6] = 0xc400;
  val.a[7] = 0xc800;

  res.x = _mm256_cvtph_ps (val.x);

  if (check_union256 (res, exp))
    abort ();
}
