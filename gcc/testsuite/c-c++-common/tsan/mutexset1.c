/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

int Global;
pthread_mutex_t mtx;

void *Thread1(void *x) {
  sleep(1);
  pthread_mutex_lock(&mtx);
  Global++;
  pthread_mutex_unlock(&mtx);
  return NULL;
}

void *Thread2(void *x) {
  Global--;
  return NULL;/* { dg-output ".*" } */

}

int main() {
  pthread_mutex_init(&mtx, 0);
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
  pthread_mutex_destroy(&mtx);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
/* { dg-output "  Read of size 4 at 0x\[0-9a-f\]+ by thread T1 \\(mutexes: write M\[0-9\]\\):.*" } */
/* { dg-output "  Previous write of size 4 at 0x\[0-9a-f\]+ by thread T2:.*" } */
/* { dg-output "  Mutex M\[0-9\] created at:.*" } */
/* { dg-output "    #0 pthread_mutex_init.*" } */
/* { dg-output "    #1 main (.*mutexset1.c|\\?{2}):\[0-9]+.*" } */
