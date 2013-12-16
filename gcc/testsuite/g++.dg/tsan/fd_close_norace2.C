/* { dg-do run } */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

int pipes[2];

void *Thread(void *x) {
  // wait for shutown signal
  while (read(pipes[0], &x, 1) != 1) {
  }
  close(pipes[0]);
  close(pipes[1]);
  return 0;
}

int main() {
  if (pipe(pipes))
    return 1;
  pthread_t t;
  pthread_create(&t, 0, Thread, 0);
  // send shutdown signal
  while (write(pipes[1], &t, 1) != 1) {
  }
  pthread_join(t, 0);
  printf("OK\n");
}

/* { dg-prune-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
/* { dg-output "OK" } */
