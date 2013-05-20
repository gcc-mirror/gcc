/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2" } */

#include "avx2-check.h"

#include "rotate-3.c"

static void
__attribute__((noinline))
avx2_test (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = i * 1073741789U;
  foo ();
  for (i = 0; i < 1024; i++)
    {
      int j = i & 31;
      unsigned int x = i * 1073741789U;
      if (a[i] != ((x << j) | (x >> ((-j) & 31))))
	abort ();
    }
}
