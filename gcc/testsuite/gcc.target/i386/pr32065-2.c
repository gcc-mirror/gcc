/* { dg-do run } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-Os -msse -std=gnu99" } */

#include "sse-check.h"

extern void abort (void);

static void
sse_test (void)
{
  if (7.999999999999999999999999999999999E6144dl + 3.0E6144dl
      != __builtin_infd32 ())
    abort ();
}
