/* { dg-do run } */
/* { dg-options "-O1 -msse2 -ftree-vectorize" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

unsigned int v1[] __attribute__ ((aligned(16))) =
{
  0x80000000, 1, 0xa0000000, 2,
  3, 0xd0000000, 0xf0000000, 0xe0000000
};
unsigned int v2[] __attribute__ ((aligned(16))) =
{
  4, 0xb0000000, 5, 0xc0000000,
  0xd0000000, 6, 7, 8
};

unsigned int max[] =
{
  0x80000000, 0xb0000000, 0xa0000000, 0xc0000000,
  0xd0000000, 0xd0000000, 0xf0000000, 0xe0000000
};

unsigned int min[] =
{
  4, 1, 5, 2,
  3, 6, 7, 8
};

unsigned int res[16] __attribute__ ((aligned(16)));

extern void abort (void);

void
find_max (void)
{
  int i;

  for (i = 0; i < 8; i++)
    res[i] = v1[i] < v2[i] ? v2[i] : v1[i];
}

void
find_min (void)
{
  int i;

  for (i = 0; i < 8; i++)
    res[i] = v1[i] > v2[i] ? v2[i] : v1[i];
}

static void
TEST (void)
{
  int i;
  int err = 0;

  find_max ();
  for (i = 0; i < 8; i++)
    if (res[i] != max[i])
      err++;

  find_min ();
  for (i = 0; i < 8; i++)
    if (res[i] != min[i])
      err++;

  if (err)
    abort ();
}
