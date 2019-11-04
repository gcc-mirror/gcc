/* { dg-shouldfail "tsan" } */

/* { dg-additional-options "-fno-unroll-loops" { target { powerpc*-*-* } } } */
/* -fno-unroll-loops help to avoid ThreadSanitizer reporting multi-times
   message for pthread_create at difference calling addresses.  */

#include <pthread.h>
#include <unistd.h>

void *Thread(void *x) {
  return 0;
}

int main() {
  int i;
  for (i = 0; i < 5; i++) {
    pthread_t t;
    pthread_create(&t, 0, Thread, 0);
  }
  sleep(1);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: thread leak.*(\n|\r\n|\r)" } */
/* { dg-output "  And 4 more similar thread leaks.*" } */
