/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mtune=generic -ffast-math" } */

#include "avx-check.h"

#define N 16
#define DIFF 242

double b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
double c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

void
main1 (double x, double max_result)
{
  int i;
  double diff = 2;
  double max = x;
  double min = 10;

  for (i = 0; i < N; i++) {
    diff += (b[i] - c[i]);
  }

  for (i = 0; i < N; i++) {
    max = max < c[i] ? c[i] : max;
  }

  for (i = 0; i < N; i++) {
    min = min > c[i] ? c[i] : min;
  }

  /* check results:  */
  if (diff != DIFF)
    abort ();
  if (max != max_result)
    abort ();
  if (min != 0)
    abort ();
}

static void
avx_test (void)
{ 
  main1 (100, 100);
  main1 (0, 15);
}
