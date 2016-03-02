#include <pthread.h>
#include <stdio.h>

void *Thread(void *x) {
  return 0;
}

int main() {
  pthread_t t;
  pthread_create(&t, 0, Thread, 0);
  pthread_join(t, 0);
  fprintf(stderr, "PASS\n");
  return 0;
}

/* { dg-prune-output "WARNING: ThreadSanitizer: thread leak.*" } */
