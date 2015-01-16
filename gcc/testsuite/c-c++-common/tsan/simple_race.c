/* { dg-set-target-env-var TSAN_OPTIONS "halt_on_error=1" } */
/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */

#include <pthread.h>
#include <unistd.h>
#include "tsan_barrier.h"

#define MAX_ITERATIONS_NUMBER 1
#define SLEEP_STEP 128000

static pthread_barrier_t barrier;
unsigned int delay_time = 1000;

static inline void delay () {
  usleep(delay_time);
}

extern int main_1();

int main() {
  barrier_init(&barrier, 2);
  int i;
  for (i = 0; i < MAX_ITERATIONS_NUMBER; i++) {
    main_1();
    delay_time += delay_time < 256000 ? delay_time : SLEEP_STEP;
  }
  return 0;
}

int Global;

void *Thread1(void *x) {
  barrier_wait(&barrier);
  delay();
  Global = 42;
  return NULL;
}

void *Thread2(void *x) {
  Global = 43;
  barrier_wait(&barrier);
  return NULL;
}

int main_1() {
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
