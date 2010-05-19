/* { dg-do run } */
/* { dg-options "-O3 -msse2" } */
/* { dg-options "-O3 -msse2 -mpe-aligned-commons" { target pe_aligned_commons } } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

int iarr[64];
int iint = 0;

void
sse2_test (void)
{
  int i;

  for (i = 0; i < 64; i++)
    iarr[i] = -2;
}
