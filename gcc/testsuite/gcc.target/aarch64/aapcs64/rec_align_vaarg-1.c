/* Test AAPCS layout (alignment of varargs) for callee.  */

/* { dg-do run { target aarch64*-*-* } } */

#include <stdarg.h>

extern void abort (void);

typedef __attribute__ ((__aligned__ (16))) long alignedlong;

void
test_pass_overaligned_long_vaargs (long l, ...)
{
  va_list va;
  va_start (va, l);
  /* Arguments should be passed in the same registers as if they were ints.  */
  while (l-- > 0)
    if (va_arg (va, long) != l)
      abort ();
  va_end (va);
}

int
main (int argc, char **argv)
{
  alignedlong a = 9;
  alignedlong b = 8;
  alignedlong c = 7;
  alignedlong d = 6;
  alignedlong e = 5;
  alignedlong f = 4;
  alignedlong g = 3;
  alignedlong h = 2;
  alignedlong i = 1;
  alignedlong j = 0;
  test_pass_overaligned_long_vaargs (a, b, c, d, e, f, g, h, i, j);
  return 0;
}
