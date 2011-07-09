/* { dg-do run } */
/* { dg-options "-mpreferred-stack-boundary=4 -msse" } */
/* { dg-require-effective-target ia32 } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

extern void abort(void);

void __attribute__((fastcall, sseregparm)) foo(int i, int j, float x)
{
  static int last_align = -1;
  int dummy, align = (int)&dummy & 15;
  if (last_align < 0)
    last_align = align;
  else if (align != last_align)
    abort ();
}

static void
sse_test (void)
{
	foo(0,0,0.0);
	foo(0,0,0.0);
}
