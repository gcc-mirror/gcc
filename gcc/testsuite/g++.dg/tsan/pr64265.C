// PR sanitizer/64265
// { dg-shouldfail "tsan" }
// { dg-additional-options "-fno-omit-frame-pointer -ldl" }

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;
int v;

__attribute__((noinline, noclone)) int
foo (int x)
{
  if (x < 99)
    throw x;
  barrier_wait (&barrier);
  v++;
  return x;
}

__attribute__((noinline, noclone)) void
bar (void)
{
  for (int i = 0; i < 100; i++)
    try
      {
	foo (i);
      }
    catch (int)
      {
      }
}

__attribute__((noinline, noclone)) void *
tf (void *)
{
  bar ();
  return NULL;
}

int
main ()
{
  pthread_t th;
  barrier_init (&barrier, 2);
  if (pthread_create (&th, NULL, tf, NULL))
    return 0;
  v++;
  barrier_wait (&barrier);
  pthread_join (th, NULL);
  return 0;
}

// { dg-output "WARNING: ThreadSanitizer: data race.*#2 _?(tf|_Z2tfPv)" }
