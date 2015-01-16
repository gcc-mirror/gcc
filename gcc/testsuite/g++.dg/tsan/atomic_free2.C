/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;

void *Thread(void *a) {
  barrier_wait(&barrier);
  __atomic_fetch_add((int*)a, 1, __ATOMIC_SEQ_CST);
  return 0;
}

int main() {
  barrier_init(&barrier, 2);
  int *a = new int(0);
  pthread_t t;
  pthread_create(&t, 0, Thread, a);
  delete a;
  barrier_wait(&barrier);
  pthread_join(t, 0);
}

/* { dg-output "WARNING: ThreadSanitizer: heap-use-after-free.*(\n|\r\n|\r)" } */
