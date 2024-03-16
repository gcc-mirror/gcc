// Test pthread_cond_clockwait not generating false positives with tsan
// { dg-do run { target { { *-*-linux* *-*-gnu* *-*-uclinux* } && pthread } } }
// { dg-options "-fsanitize=thread -lpthread" }

#include <pthread.h>

// This overloaded version should only be selected on targets that
// don't have a pthread_cond_clockwait in pthread.h, and it will wait
// indefinitely for the cond_signal that, in this testcase, ought to
// be delivered.
static inline int
pthread_cond_clockwait (pthread_cond_t *cv,
			pthread_mutex_t *mtx,
			__clockid_t,
			void const /* struct timespec */ *)
{
  return pthread_cond_wait (cv, mtx);
}		   

pthread_cond_t cv;
pthread_mutex_t mtx;

void *fn(void *vp) {
    pthread_mutex_lock(&mtx);
    pthread_cond_signal(&cv);
    pthread_mutex_unlock(&mtx);
    return NULL;
}

int main() {
    pthread_mutex_lock(&mtx);

    pthread_t tid;
    pthread_create(&tid, NULL, fn, NULL);

    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    ts.tv_sec += 10;
    pthread_cond_clockwait(&cv, &mtx, CLOCK_MONOTONIC, &ts);
    pthread_mutex_unlock(&mtx);

    pthread_join(tid, NULL);
    return 0;
}
