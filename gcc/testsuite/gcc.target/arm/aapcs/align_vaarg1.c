/* Test AAPCS layout (alignment of varargs) for callee.  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -fno-inline" } */

#include <stdarg.h>

extern void abort (void);

typedef __attribute__((aligned (8))) int alignedint;

void
foo (int i, ...)
{
  va_list va;
  va_start (va, i);
  /* Arguments should be passed in the same registers as if they were ints.  */
  while (i-- > 0)
    if (va_arg (va, int) != i)
      abort ();
  va_end (va);
}

int
main (int argc, char **argv)
{
  alignedint a = 5;
  alignedint b = 4;
  alignedint c = 3;
  alignedint d = 2;
  alignedint e = 1;
  alignedint f = 0;
  foo (a, b, c, d, e, f);
  return 0;
}
