/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

#include <emmintrin.h>

#define msk0   0
#define msk1   1

static void
sse2_test (void)
{
  union
    {
      __m128d x;
      double d[2];
    } val1;
  double res[2];
  int masks[2];
  int i;

  val1.d[0] = 23.;
  val1.d[1] = 45;

  res[0] = __builtin_ia32_vec_ext_v2df ((__v2df)val1.x, msk0);
  res[1] = __builtin_ia32_vec_ext_v2df ((__v2df)val1.x, msk1);

  masks[0] = msk0;
  masks[1] = msk1;

  for (i = 0; i < 2; i++)
    if (res[i] != val1.d [masks[i]])
      abort ();
}
