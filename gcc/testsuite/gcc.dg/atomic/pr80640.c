/* { dg-do run } */
/* { dg-options "-pthread" } */
/* { dg-require-effective-target pthread } */

#include <pthread.h>

static volatile int sem1;
static volatile int sem2;

static void *f(void *va)
{
  void **p = va;
  if (*p) return *p;
  sem1 = 1;
  while (!sem2)
    sched_yield ();
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
  // GCC used to RTL-CSE this and the first load, causing 0 to be returned
  return *p;
}

int main()
{
  void *p = 0;
  pthread_t thr;
  if (pthread_create(&thr, 0, f, &p))
    return 2;
  while (!sem1)
    sched_yield ();
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
  p = &p;
  __atomic_thread_fence(__ATOMIC_RELEASE);
  sem2 = 1;
  pthread_join(thr, &p);
  return !p;
}
