/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

#include <emmintrin.h>
#include <string.h>

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
    } val1, res[16], tmp;
  short ins[8] = { 8, 5, 9, 4, 2, 6, 1, 20 };
  int masks[8];
  int i;

  for (i = 0; i < 16; i++)
    val1.c[i] = i;

  res[0].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 0);
  res[1].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 1);
  res[2].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 2);
  res[3].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 3);
  res[4].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 4);
  res[5].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 5);
  res[6].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 6);
  res[7].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
						    ins[0], 7);

  for (i = 0; i < 8; i++)
    masks[i] = i;

  for (i = 0; i < 8; i++)
    {
      tmp.x = val1.x;
      tmp.s[masks[i]] = ins[0];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }

  for (i = 0; i < 8; i++)
    {
      res[i].x = (__m128i) __builtin_ia32_vec_set_v8hi ((__v8hi)val1.x,
							ins[i], 0);
      masks[i] = 0;
    }

  for (i = 0; i < 8; i++)
    {
      tmp.x = val1.x;
      tmp.s[masks[i]] = ins[i];
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
}
