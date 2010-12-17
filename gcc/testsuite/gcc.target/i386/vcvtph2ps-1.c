/* { dg-do run } */
/* { dg-require-effective-target f16c } */
/* { dg-options "-O2 -mf16c" } */

#include "f16c-check.h"

static void
f16c_test (void)
{
  union128i_w val; 
  union128 res;
  float exp[4];

  exp[0] = 1;
  exp[1] = -2;
  exp[2] = -1;
  exp[3] = 2;

  val.a[0] = 0x3c00;
  val.a[1] = 0xc000;
  val.a[2] = 0xbc00;
  val.a[3] = 0x4000;

  res.x = _mm_cvtph_ps (val.x);

  if (check_union128 (res, exp))
    abort ();
}
