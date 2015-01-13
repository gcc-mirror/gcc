/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;

void *Thread(void *a) {
  barrier_wait(&barrier);
  *(int*)a = 43;
  return 0;
}

int main() {
  barrier_init(&barrier, 2);
  static __thread int Var = 42;
  pthread_t t;
  pthread_create(&t, 0, Thread, &Var);
  Var = 43;
  barrier_wait(&barrier);
  pthread_join(t, 0);
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r).*" } */
/* { dg-output "  Location is TLS of main thread.(\n|\r\n|\r).*" } */
