/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

#include <emmintrin.h>

static void
sse2_test (void)
{
  union
    {
      __m128i x;
      char c[16];
      short s[8];
      int i[4];
      long long ll[2];
    } val1;
  int res[4];
  int masks[4];
  int i;

  for (i = 0; i < 16; i++)
    val1.c[i] = i;

  res[0] = __builtin_ia32_vec_ext_v4si ((__v4si)val1.x, 0);
  res[1] = __builtin_ia32_vec_ext_v4si ((__v4si)val1.x, 1);
  res[2] = __builtin_ia32_vec_ext_v4si ((__v4si)val1.x, 2);
  res[3] = __builtin_ia32_vec_ext_v4si ((__v4si)val1.x, 3);

  for (i = 0; i < 4; i++)
    masks[i] = i;

  for (i = 0; i < 4; i++)
    if (res[i] != val1.i [masks[i]])
      abort ();
}
