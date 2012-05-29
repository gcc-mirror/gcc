/* { dg-do run } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack" } */

/* This test is like split-3.c, but tests with a smaller stack frame,
   since that uses a different prologue.  */

#include <stdarg.h>
#include <stdlib.h>

/* Use a noinline function to ensure that the buffer is not removed
   from the stack.  */
static void use_buffer (char *buf) __attribute__ ((noinline));
static void
use_buffer (char *buf)
{
  buf[0] = '\0';
}

/* When using gold, the call to abort will force a stack split.  */

static void
down (int i, ...)
{
  char buf[1];
  va_list ap;

  va_start (ap, i);
  if (va_arg (ap, int) != 1
      || va_arg (ap, int) != 2
      || va_arg (ap, int) != 3
      || va_arg (ap, int) != 4
      || va_arg (ap, int) != 5
      || va_arg (ap, int) != 6
      || va_arg (ap, int) != 7
      || va_arg (ap, int) != 8
      || va_arg (ap, int) != 9
      || va_arg (ap, int) != 10)
    abort ();

  if (i > 0)
    {
      use_buffer (buf);
      down (i - 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    }
}

int
main (void)
{
  down (1000, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  return 0;
}
