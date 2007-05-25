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
  long long res[2];
  int masks[2];
  int i;

  for (i = 0; i < 16; i++)
    val1.c[i] = i;

  res[0] = __builtin_ia32_vec_ext_v2di ((__v2di)val1.x, 0);
  res[1] = __builtin_ia32_vec_ext_v2di ((__v2di)val1.x, 1);

  for (i = 0; i < 2; i++)
    masks[i] = i;

  for (i = 0; i < 2; i++)
    if (res[i] != val1.ll [masks[i]])
      abort ();
}
