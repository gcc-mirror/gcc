/* { dg-do run } */
/* { dg-options "-O1 -msse2 -ftree-vectorize" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

unsigned char v1[] __attribute__ ((aligned(16))) =
{
  0x80, 0xd0, 0x90, 0xa0, 1, 15, 10, 15,
  0xa0, 0xc0, 0xb0, 0xf0, 2, 25, 20, 35,
  3, 34, 30, 36, 0xd0, 0x80, 0xe0, 0xb0,
  0xf0, 0xe0, 0xe0, 0x80, 25, 34, 30, 40
};
unsigned char v2[] __attribute__ ((aligned(16))) =
{
  4, 44, 40, 48, 0xb0, 0x80, 0x80, 0x90,
  5, 55, 50, 51, 0xc0, 0xb0, 0xf0, 0xd0,
  0xd0, 0x80, 0xa0, 0xf0, 6, 61, 65, 68,
  7, 76, 75, 81, 0xe0, 0xf0, 0xc0, 0x90
};

unsigned char max[] =
{
  0x80, 0xd0, 0x90, 0xa0, 0xb0, 0x80, 0x80, 0x90,
  0xa0, 0xc0, 0xb0, 0xf0, 0xc0, 0xb0, 0xf0, 0xd0,
  0xd0, 0x80, 0xa0, 0xf0, 0xd0, 0x80, 0xe0, 0xb0,
  0xf0, 0xe0, 0xe0, 0x80, 0xe0, 0xf0, 0xc0, 0x90
};

unsigned char min[] =
{
  4, 44, 40, 48, 1, 15, 10, 15,
  5, 55, 50, 51, 2, 25, 20, 35,
  3, 34, 30, 36, 6, 61, 65, 68,
  7, 76, 75, 81, 25, 34, 30, 40
};

unsigned char res[32] __attribute__ ((aligned(16)));

extern void abort (void);

void
find_max (void)
{
  int i;

  for (i = 0; i < 32; i++)
    res[i] = v1[i] < v2[i] ? v2[i] : v1[i];
}

void
find_min (void)
{
  int i;

  for (i = 0; i < 32; i++)
    res[i] = v1[i] > v2[i] ? v2[i] : v1[i];
}

static void
TEST (void)
{
  int i;
  int err = 0;

  find_max ();
  for (i = 0; i < 32; i++)
    if (res[i] != max[i])
      err++;

  find_min ();
  for (i = 0; i < 32; i++)
    if (res[i] != min[i])
      err++;

  if (err)
    abort ();
}
