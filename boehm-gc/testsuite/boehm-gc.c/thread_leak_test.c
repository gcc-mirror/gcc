#ifndef GC_THREADS
# define GC_THREADS
#endif
#include "leak_detector.h"
#ifdef GC_PTHREADS
# include <pthread.h>
#else
# include <windows.h>
#endif
#include <stdio.h>

#ifdef GC_PTHREADS
  void * test(void * arg)
#else
  DWORD WINAPI test(LPVOID arg)
#endif
{
    int *p[10];
    int i;
    for (i = 0; i < 10; ++i) {
        p[i] = malloc(sizeof(int)+i);
    }
    CHECK_LEAKS();
    for (i = 1; i < 10; ++i) {
        free(p[i]);
    }
#ifdef GC_PTHREADS
    return arg;
#else
    return (DWORD)(GC_word)arg;
#endif
}

#define NTHREADS 5

int main(void) {
    int i;
#ifdef GC_PTHREADS
    pthread_t t[NTHREADS];
#else
    HANDLE t[NTHREADS];
    DWORD thread_id;
#endif
    int code;

    GC_find_leak = 1;    /* for new collect versions not compiled       */
    GC_INIT();
    for (i = 0; i < NTHREADS; ++i) {
#ifdef GC_PTHREADS
       code = pthread_create(t + i, 0, test, 0);
#else
       t[i] = CreateThread(NULL, 0, test, 0, 0, &thread_id);
       code = t[i] != NULL ? 0 : (int)GetLastError();
#endif
       if (code != 0) {
          printf("Thread creation failed %d\n", code);
        }
    }
    for (i = 0; i < NTHREADS; ++i) {
#ifdef GC_PTHREADS
       code = pthread_join(t[i], 0);
#else
       code = WaitForSingleObject(t[i], INFINITE) == WAIT_OBJECT_0 ? 0 :
                                                        (int)GetLastError();
#endif
       if (code != 0) {
          printf("Thread join failed %d\n", code);
        }
    }
    CHECK_LEAKS();
    CHECK_LEAKS();
    CHECK_LEAKS();
    return 0;
}
