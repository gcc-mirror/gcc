/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -msse -mfpmath=sse -mrecip" } */
/* { dg-require-effective-target sse } */
#include "sse-check.h"

extern float sqrtf (float);
float __attribute__((noinline)) broken (float a, float b)
{
  return sqrtf (a / b);
}

extern void abort (void);
extern void *memcpy (void *, const void *, __SIZE_TYPE__);
static void
sse_test (void)
{
  int i;
  float x;
  char buf[sizeof (float)];
  x = broken (0.0f, 10000.0f);
  /* A convoluted way to check for the correct result (zero) for all
     floating point formats.
     We can't use ==, !=, or range checks, or isinf/isnan/isunordered,
     because all of these will not do the right thing under -ffast-math,
     as they can assume that neither nan nor inf are returned.  */
  memcpy (&buf, &x, sizeof (float));
  for (i = 0; i < sizeof (float); i++)
    if (buf[i] != 0)
      abort ();
}
