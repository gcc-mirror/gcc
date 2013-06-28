/* This test needs to use setrlimit to set the stack size, so it can
   only run on Unix.  */
/* { dg-do run { target *-*-linux* *-*-gnu* *-*-solaris* *-*-darwin* } } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack" } */

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/resource.h>

/* Use a noinline function to ensure that the buffer is not removed
   from the stack.  */
static void use_buffer (char *buf, size_t) __attribute__ ((noinline));
static void
use_buffer (char *buf, size_t c)
{
  size_t i;

  for (i = 0; i < c; ++i)
    buf[i] = (char) i;
}

/* Each recursive call uses 10 * i bytes.  We call it 1000 times,
   using a total of 5,000,000 bytes.  If -fsplit-stack is not working,
   that will overflow our stack limit.  */

static void
down1 (int i)
{
  char buf[10 * i];

  if (i > 0)
    {
      use_buffer (buf, 10 * i);
      down1 (i - 1);
    }
}

/* Same thing, using alloca.  */

static void
down2 (int i)
{
  char *buf = alloca (10 * i);

  if (i > 0)
    {
      use_buffer (buf, 10 * i);
      down2 (i - 1);
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
  down1 (1000);
  down2 (1000);
  return 0;
}
