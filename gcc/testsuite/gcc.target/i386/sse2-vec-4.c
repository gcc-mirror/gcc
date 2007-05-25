/* { dg-do run { target i?86-*-* x86_64-*-* } } */
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
  short res[8];
  int masks[8];
  int i;

  for (i = 0; i < 16; i++)
    val1.c[i] = i;

  res[0] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 0);
  res[1] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 1);
  res[2] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 2);
  res[3] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 3);
  res[4] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 4);
  res[5] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 5);
  res[6] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 6);
  res[7] = __builtin_ia32_vec_ext_v8hi ((__v8hi)val1.x, 7);

  for (i = 0; i < 8; i++)
    masks[i] = i;

  for (i = 0; i < 8; i++)
    if (res[i] != val1.s [masks[i]])
      abort ();
}
