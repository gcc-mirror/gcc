/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#define N 2

int a[N] = {-287807, 604344};
int b[N] = {474362, 874120};
int r[N];

int rc[N] = {914249338, -11800128};

static void
sse2_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    r[i] = a[i] * b[i];

  /* check results:  */
  for (i = 0; i < N; i++)
    if (r[i] != rc[i])
      abort ();
}
