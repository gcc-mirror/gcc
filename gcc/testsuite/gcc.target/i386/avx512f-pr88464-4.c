/* PR tree-optimization/88464 */
/* { dg-do run { target { avx512f } } } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512 -mtune=skylake-avx512" } */

#include "avx512f-check.h"

#include "avx512f-pr88464-3.c"

static void
avx512f_test (void)
{
  double a[1024], b[1024];
  float c[1024], f[1024];
  int d[1024];
  long e[1024];
  int i;
  for (i = 0; i < 1024; i++)
    {
      asm volatile ("" : "+g" (i));
      a[i] = -5.0;
      b[i] = (i % 3) != 0 ? 2.0 * i : -5.0;
      d[i] = (i % 3) != 0 ? 1023 - i : __INT_MAX__;
    }
  f1 (a, b, d, 1024);
  for (i = 0; i < 1024; i++)
    {
      asm volatile ("" : "+g" (i));
      if (a[i] != ((i % 3) != 0 ? (1023 - i) * 2.0 : -5.0))
	abort ();
      a[i] = -5.0;
      b[i] = (i % 3) != 1 ? 3.0 * i : -5.0;
      e[i] = (i % 3) != 1 ? 1023 - i : __LONG_MAX__;
    }
  f2 (a, b, e, 1024);
  for (i = 0; i < 1024; i++)
    {
      asm volatile ("" : "+g" (i));
      if (a[i] != ((i % 3) != 2 ? (1023 - i) * 3.0 : -5.0))
	abort ();
      c[i] = -5.0f;
      d[i] = (i % 3) != 2 ? 1023 - i : __INT_MAX__;
      f[i] = (i % 3) != 2 ? 4.0f * i : -5.0f;
    }
  f3 (c, f, d, 1024);
  for (i = 0; i < 1024; i++)
    {
      asm volatile ("" : "+g" (i));
      if (c[i] != ((i % 3) != 1 ? (1023 - i) * 4.0f : -5.0f))
	abort ();
      c[i] = -5.0f;
      e[i] = (i % 3) != 0 ? 1023 - i : __INT_MAX__;
      f[i] = (i % 3) != 0 ? 5.0f * i : -5.0f;
    }
  f4 (c, f, e, 1024);
  for (i = 0; i < 1024; i++)
    {
      asm volatile ("" : "+g" (i));
      if (c[i] != ((i % 3) != 0 ? (1023 - i) * 5.0f : -5.0f))
	abort ();
    }
}
