/* PR middle-end/36858 */
/* { dg-do run } */
/* { dg-options "-w" { target { ! ia32 } } } */
/* { dg-options "-w" { target { llp64 } } } */
/* { dg-options "-w -msse2 -mpreferred-stack-boundary=2" { target { ia32 } } } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"
#include <stdarg.h>
#include <emmintrin.h>

int
__attribute__((noinline))
test (int a, ...)
{
  return a;
}

__m128 n1 = { -283.3, -23.3, 213.4, 1119.03 };

int
__attribute__((noinline))
foo (void)
{
  return test (1, n1);
}

static void
__attribute__((noinline))
sse2_test (void)
{
  if (foo () != 1)
    abort ();
}
