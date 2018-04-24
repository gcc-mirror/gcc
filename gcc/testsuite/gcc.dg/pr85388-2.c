/* { dg-do run { target { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } } */
/* { dg-require-effective-target cet } */
/* { dg-require-effective-target split_stack } */
/* { dg-require-effective-target pthread_h } */
/* { dg-options "-pthread -fsplit-stack -fcf-protection" } */

#include <stdlib.h>
#include <pthread.h>

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

static void *
thread_routine (void *arg __attribute__ ((unused)))
{
  down (1000);
  return NULL;
}

int
main (void)
{
  int i;
  pthread_t tid;
  void *dummy;

  i = pthread_create (&tid, NULL, thread_routine, NULL);
  if (i != 0)
    abort ();
  i = pthread_join (tid, &dummy);
  if (i != 0)
    abort ();
  return 0;
}
