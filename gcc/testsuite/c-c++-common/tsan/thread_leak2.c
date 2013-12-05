/* { dg-do run } */
/* { dg-shouldfail "tsan" } */
/* { dg-skip-if "" { *-*-* }  { "-O3 -funroll-loops" "-O3 -funroll-all-loops" } { "" } } */

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
