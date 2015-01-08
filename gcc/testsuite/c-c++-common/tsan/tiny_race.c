/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;
int Global;

void *Thread1(void *x) {
  barrier_wait(&barrier);
  Global = 42;
  return x;
}

int main() {
  barrier_init(&barrier, 2);
  pthread_t t;
  pthread_create(&t, 0, Thread1, 0);
  Global = 43;
  barrier_wait(&barrier);
  pthread_join(t, 0);
  return Global;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
