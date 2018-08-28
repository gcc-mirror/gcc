/* { dg-do run { target pthread_h } } */
/* { dg-skip-if "no pthread_barrier" { *-*-darwin* } } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=2:halt_on_error=1" } */
/* { dg-options "-fsanitize=address,pointer-subtract" } */
/* { dg-additional-options "-pthread" { target pthread } } */

#include <unistd.h>
#include <pthread.h>

char *pointers[2];
pthread_barrier_t bar;

void *
thread_main (void *n)
{
  char local;

  __UINTPTR_TYPE__ id = (__UINTPTR_TYPE__) n;
  pointers[id] = &local;
  pthread_barrier_wait (&bar);
  pthread_barrier_wait (&bar);

  return 0;
}

int
main ()
{
  pthread_t threads[2];
  pthread_barrier_init (&bar, NULL, 3);
  pthread_create (&threads[0], NULL, thread_main, (void *) 0);
  pthread_create (&threads[1], NULL, thread_main, (void *) 1);
  pthread_barrier_wait (&bar);

  /* This case is not handled yet.  */
  volatile __PTRDIFF_TYPE__ r = pointers[0] - pointers[1];

  pthread_barrier_wait (&bar);
  pthread_join (threads[0], NULL);
  pthread_join (threads[1], NULL);
  pthread_barrier_destroy (&bar);

  return 0;
}
