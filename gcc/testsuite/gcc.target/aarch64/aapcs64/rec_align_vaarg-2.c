/* Test AAPCS layout (alignment of varargs) for callee.  */

/* { dg-do run { target aarch64*-*-* } } */

#include <stdarg.h>

extern void abort (void);

typedef __attribute__ ((__aligned__ (16))) int alignedint;

void
test_pass_overaligned_int_vaargs (int i, ...)
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
  test_pass_overaligned_int_vaargs (9, 8, 7, 6, 5, 4, 3, 2, 1, 0);
  return 0;
}
