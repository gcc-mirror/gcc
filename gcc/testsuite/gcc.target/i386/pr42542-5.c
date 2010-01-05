/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O1 -msse4.2 -ftree-vectorize" } */

#include "sse4_2-check.h"

long long v1[] __attribute__ ((aligned(16))) =
{
  -3, 2, 3, -4 
};
long long v2[] __attribute__ ((aligned(16))) =
{
  4, -10, -20, 6
};

long long max[] =
{
  4, 2, 3, 6
};

long long min[] =
{
  -3, -10, -20, -4
};

long long res[4] __attribute__ ((aligned(16)));

extern void abort (void);

void
find_max (void)
{
  int i;

  for (i = 0; i < 4; i++)
    res[i] = v1[i] < v2[i] ? v2[i] : v1[i];
}

void
find_min (void)
{
  int i;

  for (i = 0; i < 4; i++)
    res[i] = v1[i] > v2[i] ? v2[i] : v1[i];
}

static void
sse4_2_test (void)
{
  int i;
  int err = 0;

  find_max ();
  for (i = 0; i < 4; i++)
    if (res[i] != max[i])
      err++;

  find_min ();
  for (i = 0; i < 4; i++)
    if (res[i] != min[i])
      err++;

  if (err)
    abort ();
}
