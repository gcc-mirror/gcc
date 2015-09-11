/* PR sanitizer/65400 */
/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-fno-omit-frame-pointer -ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;
int v;

int
fn1 (int a, int b, int c)
{
  int r = (a ^ b) % c;
  r = r * a * b + c;
  r = (r ^ b) % c;
  return r;
}

int
fn2 (int a, int b, int c)
{
  int r = (a ^ b) % c;
  r = r * a * b + c;
  r = (r ^ b) % c;
  return r;
}

__attribute__((noinline, noclone)) void
foo (void)
{
  barrier_wait (&barrier);
  barrier_wait (&barrier);
  v++;
}

__attribute__((noinline, noclone)) void
bar (void)
{
  int (*fna) (int, int, int);
  int (*fnb) (int, int, int);
  int i;
  asm ("" : "=g" (fna) : "0" (fn1));
  asm ("" : "=g" (fnb) : "0" (fn2));
  for (i = 0; i < 128; i++)
    {
      fna (0, 0, i + 1);
      fnb (0, 0, i + 1);
    }
  foo ();
}

__attribute__((noinline, noclone)) void *
tf (void *arg)
{
  (void) arg;
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
  barrier_wait (&barrier);
  v++;
  barrier_wait (&barrier);
  pthread_join (th, NULL);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*#2 _?tf" } */
