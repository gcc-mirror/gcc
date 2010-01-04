/* { dg-do run } */
/* { dg-options "-O1 -msse2 -ftree-vectorize" } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

unsigned short v1[] __attribute__ ((aligned(16))) =
{
  0x8000, 0x9000, 1, 10, 0xa000, 0xb000, 2, 20,
  3, 30, 0xd000, 0xe000, 0xf000, 0xe000, 25, 30
};
unsigned short v2[] __attribute__ ((aligned(16))) =
{
  4, 40, 0xb000, 0x8000, 5, 50, 0xc000, 0xf000,
  0xd000, 0xa000, 6, 65, 7, 75, 0xe000, 0xc000
};

unsigned short max[] =
{
  0x8000, 0x9000, 0xb000, 0x8000, 0xa000, 0xb000, 0xc000, 0xf000,
  0xd000, 0xa000, 0xd000, 0xe000, 0xf000, 0xe000, 0xe000, 0xc000
};

unsigned short min[] =
{
  4, 40, 1, 10, 5, 50, 2, 20,
  3, 30, 6, 65, 7, 75, 25, 30
};

unsigned short res[16] __attribute__ ((aligned(16)));

extern void abort (void);

void
find_max (void)
{
  int i;

  for (i = 0; i < 16; i++)
    res[i] = v1[i] < v2[i] ? v2[i] : v1[i];
}

void
find_min (void)
{
  int i;

  for (i = 0; i < 16; i++)
    res[i] = v1[i] > v2[i] ? v2[i] : v1[i];
}

static void
TEST (void)
{
  int i;
  int err = 0;

  find_max ();
  for (i = 0; i < 16; i++)
    if (res[i] != max[i])
      err++;

  find_min ();
  for (i = 0; i < 16; i++)
    if (res[i] != min[i])
      err++;

  if (err)
    abort ();
}
