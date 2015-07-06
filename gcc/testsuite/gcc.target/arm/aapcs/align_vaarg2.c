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
  /* alignedint should be pulled out of regs/stack just like an int.  */
  while (i-- > 0)
    if (va_arg (va, alignedint) != i)
      abort ();
  va_end (va);
}

int
main (int argc, char **argv)
{
  foo (5, 4, 3, 2, 1, 0);
  return 0;
}
