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
  char res[16];
  int masks[16];
  int i;

  for (i = 0; i < 16; i++)
    val1.c[i] = i;

  res[0] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 0);
  res[1] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 1);
  res[2] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 2);
  res[3] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 3);
  res[4] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 4);
  res[5] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 5);
  res[6] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 6);
  res[7] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 7);
  res[8] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 8);
  res[9] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 9);
  res[10] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 10);
  res[11] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 11);
  res[12] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 12);
  res[13] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 13);
  res[14] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 14);
  res[15] = __builtin_ia32_vec_ext_v16qi ((__v16qi)val1.x, 15);

  for (i = 0; i < 16; i++)
    masks[i] = i;

  for (i = 0; i < 16; i++)
    if (res[i] != val1.c [masks[i]])
      abort ();
}
