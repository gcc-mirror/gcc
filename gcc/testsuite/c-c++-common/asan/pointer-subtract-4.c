/* { dg-do run { target pthread_h } } */
/* { dg-skip-if "no pthread_barrier" { *-*-darwin* } } */
/* { dg-shouldfail "asan" } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=2:halt_on_error=1" } */
/* { dg-options "-fsanitize=address,pointer-subtract" } */
/* { dg-additional-options "-pthread" { target pthread } } */

#include <unistd.h>
#include <pthread.h>

char *pointer;
pthread_barrier_t bar;

void *
thread_main (void *n)
{
  char local;
  (void) n;
  pointer = &local;
  pthread_barrier_wait (&bar);
  pthread_barrier_wait (&bar);

  return 0;
}

int
main ()
{
  pthread_t thread;
  pthread_barrier_init (&bar, NULL, 2);
  pthread_create (&thread, NULL, thread_main, NULL);
  pthread_barrier_wait (&bar);

  char local;
  char *parent_pointer = &local;

  /* { dg-output "ERROR: AddressSanitizer: invalid-pointer-pair" } */
  volatile __PTRDIFF_TYPE__ r = parent_pointer - pointer;
  pthread_barrier_wait (&bar);
  pthread_join (thread, NULL);
  pthread_barrier_destroy (&bar);

  return 0;
}
