/* { dg-do run } */
/* { dg-set-target-env-var TSAN_OPTIONS "halt_on_error=1" } */
/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

#define MAX_ITERATIONS_NUMBER 100
#define SLEEP_STEP 128000 

unsigned int delay_time = 1000;

static inline void delay () {
  usleep(delay_time);
}

extern int main_1();

int main() {
  int i;
  for (i = 0; i < MAX_ITERATIONS_NUMBER; i++) {
    main_1();
    delay_time += delay_time < 256000 ? delay_time : SLEEP_STEP;
  }
  return 0;
}

int Global;

void *Thread1(void *x) {
  delay();
  Global = 42;
  return NULL;
}

void *Thread2(void *x) {
  Global = 43;
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
