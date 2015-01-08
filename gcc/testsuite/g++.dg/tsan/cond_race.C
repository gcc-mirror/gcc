/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-ldl" } */
/* { dg-output "ThreadSanitizer: data race.*" } */
/* { dg-output "pthread_cond_signal.*" } */

#include <pthread.h>
#include "tsan_barrier.h"

static pthread_barrier_t barrier;

struct Ctx {
  pthread_mutex_t m;
  pthread_cond_t c;
  bool done;
};

void *thr(void *p) {
  Ctx *c = (Ctx*)p;
  pthread_mutex_lock(&c->m);
  c->done = true;
  pthread_mutex_unlock(&c->m);
  pthread_cond_signal(&c->c);
  barrier_wait(&barrier);
  return 0;
}

int main() {
  barrier_init(&barrier, 2);
  Ctx *c = new Ctx();
  pthread_mutex_init(&c->m, 0);
  pthread_cond_init(&c->c, 0);
  pthread_t th;
  pthread_create(&th, 0, thr, c);
  pthread_mutex_lock(&c->m);
  while (!c->done)
    pthread_cond_wait(&c->c, &c->m);
  pthread_mutex_unlock(&c->m);
  barrier_wait(&barrier);
  delete c;
  pthread_join(th, 0);
}
