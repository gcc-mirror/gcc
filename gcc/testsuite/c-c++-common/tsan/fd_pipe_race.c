/* { dg-do run } */
/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

int fds[2];

void *Thread1(void *x) {
  write(fds[1], "a", 1);
  return NULL;
}

void *Thread2(void *x) {
  sleep(1);
  close(fds[0]);
  close(fds[1]);
  return NULL;
}

int main() {
  pipe(fds);
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*\n" } */
/* { dg-output "  Write of size 8.*\n" } */
/* { dg-output "    #0 close.*\n" } */
/* { dg-output "    #1 Thread2.*\n" } */
/* { dg-output "  Previous read of size 8.*\n" } */
/* { dg-output "    #0 write.*\n" } */
/* { dg-output "    #1 Thread1.*\n" } */
