/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;
pthread_rwlock_t rwlock;
int GLOB;

void *Thread1(void *p) {
 (void)p;
  pthread_rwlock_rdlock(&rwlock);
  // Write under reader lock.
  barrier_wait(&barrier);
  GLOB++;
  pthread_rwlock_unlock(&rwlock);
  return 0;
}

int main(int argc, char *argv[]) {
  barrier_init(&barrier, 2);
  pthread_rwlock_init(&rwlock, NULL);
  pthread_rwlock_rdlock(&rwlock);
  pthread_t t;
  pthread_create(&t, 0, Thread1, 0);
  volatile int x = GLOB;
 (void)x;
  pthread_rwlock_unlock(&rwlock);
  barrier_wait(&barrier);
  pthread_join(t, 0);
  pthread_rwlock_destroy(&rwlock);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
/* { dg-output "  Write of size 4 at 0x\[0-9a-f\]+ by thread T1.*:(\n|\r\n|\r).*" } */
/* { dg-output "    #0 Thread1.*\(write_in_reader_lock.c|\\?{2}\):\[0-9\]+ .*" } */
/* { dg-output "  Previous read of size 4 at.* by main thread.*:(\n|\r\n|\r).*" } */
/* { dg-output "    #0 main.*\(write_in_reader_lock.c|\\?{2}\):\[0-9\]+.*" } */
