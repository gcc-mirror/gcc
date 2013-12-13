/* { dg-do run } */
/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <stddef.h>

void *Thread(void *a) {
  *(int*)a = 43;
  return 0;
}

int main() {
  static __thread int Var = 42;
  pthread_t t;
  pthread_create(&t, 0, Thread, &Var);
  Var = 43;
  pthread_join(t, 0);
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r).*" } */
/* { dg-output "  Location is TLS of main thread.(\n|\r\n|\r).*" } */
