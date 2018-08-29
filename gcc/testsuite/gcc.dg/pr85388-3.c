/* This test needs to use setrlimit to set the stack size, so it can
   only run on Unix.  */
/* { dg-do run { target { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } } */
/* { dg-require-effective-target cet } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack -fcf-protection" } */

#include <stdarg.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/resource.h>

/* Use a noinline function to ensure that the buffer is not removed
   from the stack.  */
static void use_buffer (char *buf) __attribute__ ((noinline));
static void
use_buffer (char *buf)
{
  buf[0] = '\0';
}

/* Each recursive call uses 10,000 bytes.  We call it 1000 times,
   using a total of 10,000,000 bytes.  If -fsplit-stack is not
   working, that will overflow our stack limit.  */

static void
down (int i, ...)
{
  char buf[10000];
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
  struct rlimit r;

  /* We set a stack limit because we are usually invoked via make, and
     make sets the stack limit to be as large as possible.  */
  r.rlim_cur = 8192 * 1024;
  r.rlim_max = 8192 * 1024;
  if (setrlimit (RLIMIT_STACK, &r) != 0)
    abort ();
  down (1000, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  return 0;
}
