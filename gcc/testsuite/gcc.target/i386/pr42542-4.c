/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O1 -msse4.2 -ftree-vectorize" } */

#include "sse4_2-check.h"

unsigned long long v1[] __attribute__ ((aligned(16))) =
{
  0x8000000000000000ULL, 2,
  3, 0xd000000000000000ULL
};
unsigned long long v2[] __attribute__ ((aligned(16))) =
{
  4, 0xb000000000000000ULL,
  0xf000000000000000ULL, 6
};

unsigned long long max[] =
{
  0x8000000000000000ULL, 0xb000000000000000ULL,
  0xf000000000000000ULL, 0xd000000000000000ULL
};

unsigned long long min[] =
{
  4, 2,
  3, 6
};

unsigned long long res[4] __attribute__ ((aligned(16)));

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
