/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;
pthread_mutex_t Mtx;
int Global;

void *Thread1(void *x) {
  pthread_mutex_init(&Mtx, 0);
  pthread_mutex_lock(&Mtx);
  Global = 42;
  pthread_mutex_unlock(&Mtx);
  barrier_wait(&barrier);
  return NULL;
}

void *Thread2(void *x) {
  barrier_wait(&barrier);
  pthread_mutex_lock(&Mtx);
  Global = 43;
  pthread_mutex_unlock(&Mtx);
  return NULL;
}

int main() {
  barrier_init(&barrier, 2);
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
  pthread_mutex_destroy(&Mtx);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
/* { dg-output "  Atomic read of size 1 at .* by thread T2:(\n|\r\n|\r)" } */
/* { dg-output "    #0 pthread_mutex_lock.*" } */
/* { dg-output "    #1 Thread2.* .*(race_on_mutex.c:22|\\?{2}:0) (.*)" } */
/* { dg-output "  Previous write of size 1 at .* by thread T1:(\n|\r\n|\r)" } */
/* { dg-output "    #0 pthread_mutex_init .* (.)*" } */
/* { dg-output "    #1 Thread1.* .*(race_on_mutex.c:12|\\?{2}:0) .*" } */
