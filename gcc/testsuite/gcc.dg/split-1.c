/* This test needs to use setrlimit to set the stack size, so it can
   only run on Unix.  */
/* { dg-do run { target *-*-linux* *-*-gnu* *-*-solaris* *-*-darwin* } } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack" } */

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
down (int i)
{
  char buf[10000];

  if (i > 0)
    {
      use_buffer (buf);
      down (i - 1);
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
  down (1000);
  return 0;
}
