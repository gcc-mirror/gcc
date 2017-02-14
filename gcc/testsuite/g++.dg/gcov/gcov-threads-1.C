/* { dg-options "-fprofile-arcs -ftest-coverage -pthread -fprofile-update=atomic" } */
/* { dg-do run { target native } } */
/* { dg-require-effective-target profile_update_atomic } */

#include <stdint.h>
#include <pthread.h>
#include <assert.h>

#define NR 5

pthread_mutex_t cndMs[NR];
static void *ContentionNoDeadlock_thread(void *start)
{
  for (uint32_t k = 0; k < 100000; ++k)		/* count(500005) */
    {
      int starti = *(int*)start;		/* count(500000) */
      for (uint32_t i = starti; i < NR; ++i) 
	pthread_mutex_lock (&cndMs[i]);
      for (int32_t i = NR - 1; i >= starti; --i)
	pthread_mutex_unlock (&cndMs[i]);
  }
}
int main(int argc, char **argv) {
  for (unsigned i = 0; i < NR; i++)
    cndMs[i] = PTHREAD_MUTEX_INITIALIZER;

  pthread_t t[NR];
  int ids[NR];

  for (int i = 0; i < NR; i++)
  {
    ids[i] = i;
    int r = pthread_create (&t[i], NULL, ContentionNoDeadlock_thread, &ids[i]);
    assert (r == 0);				/* count(5) */
  }

  int ret;
  for (int i = 0; i < NR; i++)
    {
      int r = pthread_join (t[i], (void**)&ret);
      assert (r == 0);				/* count(5) */
    }

  return 0;					/* count(1) */
}

/* { dg-final { run-gcov gcov-threads-1.C } } */
