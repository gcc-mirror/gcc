/* { dg-do run } */
/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <unistd.h>

int Global;

void *Thread1(void *x) {
  sleep(1);
  __atomic_fetch_add(&Global, 1, __ATOMIC_RELAXED);
  return NULL;
}

void *Thread2(void *x) {
  Global++;
  return NULL;
}

int main() {
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
/* { dg-output "  Atomic write of size 4.*" } */
/* { dg-output "    #0 __tsan_atomic32_fetch_add.*" } */
/* { dg-output "    #1 Thread1.*" } */
