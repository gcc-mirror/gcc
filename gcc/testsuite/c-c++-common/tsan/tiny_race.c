/* { dg-do run } */
/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <unistd.h>

int Global;

void *Thread1(void *x) {
  sleep(1);
  Global = 42;
  return x;
}

int main() {
  pthread_t t;
  pthread_create(&t, 0, Thread1, 0);
  Global = 43;
  pthread_join(t, 0);
  return Global;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
