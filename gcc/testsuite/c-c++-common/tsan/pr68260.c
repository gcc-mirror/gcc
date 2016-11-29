/* PR sanitizer/68260 */

#include <pthread.h>
#include <stdbool.h>

bool lock;
int counter;

void *
tf (void *arg)
{
  (void) arg;
  while (__atomic_test_and_set (&lock, __ATOMIC_ACQUIRE))
    ;
  ++counter;
  __atomic_clear (&lock, __ATOMIC_RELEASE);
  return (void *) 0;
}

int
main ()
{
  pthread_t thr;
  pthread_create (&thr, 0, tf, 0);
  tf ((void *) 0);
  pthread_join (thr, 0);
  return 0;
}
