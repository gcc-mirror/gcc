/* { dg-do run } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-pthread" } */

#include <pthread.h>

char x, x1, x2;

void *
work1 (void *)
{
  for (int i = 0; i < 100; i++)
    x1 = __atomic_exchange_n (&x, x1, __ATOMIC_SEQ_CST);
  return NULL;
}

void *
work2 (void *)
{
  for (int i = 0; i < 100; i++)
    x2 = __atomic_exchange_n (&x, x2, __ATOMIC_SEQ_CST);
  return NULL;
}

void
test (void)
{
  x = 0;
  x1 = 1;
  x2 = 2;
  pthread_t w1, w2;
  if (pthread_create (&w1, NULL, work1, NULL) != 0)
    __builtin_abort ();
  if (pthread_create (&w2, NULL, work2, NULL) != 0)
    __builtin_abort ();
  if (pthread_join (w1, NULL) != 0)
    __builtin_abort ();
  if (pthread_join (w2, NULL) != 0)
    __builtin_abort ();
  if ((x ^ x1 ^ x2) != 3)
    __builtin_abort ();
}

int
main ()
{
  int i;
  for (i = 0; i < 10000; i++)
    test ();
}
