/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -mavx" } */

#include "avx-check.h"

#include "rotate-4.c"

static void
__attribute__((noinline))
avx_test (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = i * 1073741789U;
  foo (3);
  for (i = 0; i < 1024; i++)
    {
      unsigned int x = i * 1073741789U;
      if (a[i] != ((x << 3) | (x >> ((-3) & 31))))
	abort ();
    }
  foo (0);
  for (i = 0; i < 1024; i++)
    {
      unsigned int x = i * 1073741789U;
      if (a[i] != ((x << 3) | (x >> ((-3) & 31))))
	abort ();
    }
  foo (29);
  for (i = 0; i < 1024; i++)
    if (a[i] != i * 1073741789U)
      abort ();
}
