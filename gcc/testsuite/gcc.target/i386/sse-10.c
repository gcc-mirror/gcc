/* PR 17930 */
/* { dg-do run } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -mno-accumulate-outgoing-args -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer" } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -fno-omit-frame-pointer" { target *-*-mingw* *-*-cygwin* } } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

typedef _Complex double complex_16;

void __attribute__((noinline))
test (complex_16 a[5][5])
{
  int i, j, k;
  complex_16 x;

  for (j = 0; j < 5; j++)
    for (i = 0; i < 5; i++)
      {
        for (k = 0; k < j - 1; ++k)
	  x = a[k][i] * ~a[k][j];
	a[j][i] = x;
      }
}

static void
sse2_test (void)
{
  static complex_16 work[5][5];

  test (work); 
}
