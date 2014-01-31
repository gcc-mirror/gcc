/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <unistd.h>

void *Thread(void *a) {
  __atomic_fetch_add((int*)a, 1, __ATOMIC_SEQ_CST);
  return 0;
}

int main() {
  int *a = new int(0);
  pthread_t t;
  pthread_create(&t, 0, Thread, a);
  sleep(1);
  delete a;
  pthread_join(t, 0);
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
