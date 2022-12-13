#include <immintrin.h>
#include <stdint.h>
#include <pthread.h>
#include <string.h>
#ifdef DEBUG
#include <stdio.h>
#endif
#include "cpuid.h"

typedef struct {
  uint32_t id; /* filled in by launch_threads. */
} state_t;

static pthread_t* threads = 0;
static state_t* thread_state = 0;
static const unsigned int num_threads = 4;

static void* threads_worker (state_t *tstate);

void launch_threads (uint32_t nthreads,
                     void* (*worker)(state_t*),
                     state_t* tstate_proto)
{
  int i;
  thread_state = malloc (sizeof (state_t) *nthreads);
  threads = malloc (sizeof (pthread_t) *nthreads);
  memset (threads, 0, sizeof (pthread_t) *nthreads);
  for(i = 0; i < nthreads; i++)
    {
      memcpy (thread_state + i, tstate_proto, sizeof (state_t));
      thread_state[i].id = i;
      pthread_create (threads+i, NULL,
                      (void* (*)(void*))worker,  
		      (void*) (thread_state+i));
    }
}

void wait()
{
  int i;
  for(i = 0; i < num_threads; i++)
    pthread_join (threads[i], 0);
  free (threads);
  threads = 0;
  free (thread_state);
  thread_state  = 0;
}

#ifndef DO_TEST
#define DO_TEST do_test
static void rao_test (void);
__attribute__ ((noinline))
static void
do_test (void)
{
  state_t tstate_proto;
  launch_threads(num_threads, threads_worker, &tstate_proto);
  wait();
  rao_test ();
}
#endif

int
main()
{
  if (__builtin_cpu_supports ("raoint"))
    {
      DO_TEST ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    printf ("SKIPPED\n");
#endif

  return 0;
}
