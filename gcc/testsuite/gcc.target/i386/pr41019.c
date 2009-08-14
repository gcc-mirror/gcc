/* { dg-do run } */
/* { dg-options "-O2 -msse2 -ftree-vectorize" } */

#include "sse2-check.h"

long long int a[64];

void
sse2_test (void)
{
  int k;

  for (k = 0; k < 64; k++)
    a[k] = a[k] != 5 ? 12 : 10;

  for (k = 0; k < 64; k++)
    if (a[k] != 12)
      abort ();
}
