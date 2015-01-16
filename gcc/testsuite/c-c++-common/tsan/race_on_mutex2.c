/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;

void *Thread(void *x) {
  pthread_mutex_lock((pthread_mutex_t*)x);
  pthread_mutex_unlock((pthread_mutex_t*)x);
  barrier_wait(&barrier);
  return 0;
}

int main() {
  barrier_init(&barrier, 2);
  pthread_mutex_t Mtx;
  pthread_mutex_init(&Mtx, 0);
  pthread_t t;
  pthread_create(&t, 0, Thread, &Mtx);
  barrier_wait(&barrier);
  pthread_mutex_destroy(&Mtx);
  pthread_join(t, 0);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
