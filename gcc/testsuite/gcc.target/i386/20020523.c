/* PR target/6753
   This testcase was miscompiled because sse_mov?fcc_const0*
   patterns were missing earlyclobber.  */

/* { dg-do run } */
/* { dg-options "-O2 -msse -mfpmath=sse -ffast-math" } */

#include "sse-check.h"

float one = 1.f;

void bar (float f)
{
  if (__builtin_memcmp (&one, &f, sizeof (float)))
    abort ();
}

float foo (void)
{
  return 1.f;
}

typedef struct
{
  float t;
} T;

static void
sse_test (void)
{
  int i;
  T x[1];

  for (i = 0; i < 1; i++)
    {
      x[i].t = foo ();
      x[i].t = 0.f > x[i].t ? 0.f : x[i].t;
      bar (x[i].t);
    }
}
