/* Test C23 storage class specifiers in compound literals.  Thread-local
   cases, execution tests.  */
/* { dg-do run } */
/* { dg-options "-pthread -std=gnu23 -pedantic-errors" } */
/* { dg-require-effective-target pthread_h } */
/* { dg-require-effective-target pthread } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

#include <pthread.h>

extern void abort (void);
extern void exit (int);

int *
thread_addr ()
{
  return (static thread_local int []) { 1, 2 };
}

int *volatile p, *volatile q, r;

void *
thread_fn (void *)
{
  q = thread_addr ();
  if (q[0] != 1 || q[1] != 2)
    return NULL;
  q[0] = 5;
  q[1] = 6;
  return &r;
}

int
main ()
{
  int i;
  pthread_t tid;
  void *ret;
  p = thread_addr ();
  if (p[0] != 1 || p[1] != 2)
    abort ();
  p[0] = 3;
  p[1] = 4;
  if (p != thread_addr ())
    abort ();
  i = pthread_create (&tid, NULL, thread_fn, NULL);
  if (p != thread_addr ())
    abort ();
  i = pthread_join (tid, &ret);
  if (i != 0)
    abort ();
  if (ret != &r)
    abort ();
  if (p != thread_addr ())
    abort ();
  if (p[0] != 3 || p[1] != 4)
    abort ();
  exit (0);
}
