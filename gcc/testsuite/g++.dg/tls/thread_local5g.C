// Test for cleanups in the main thread, too.

// { dg-do run }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target tls_runtime }
// { dg-require-effective-target pthread }
// { dg-require-cxa-atexit "" }
// { dg-options -pthread }
// { dg-add-options tls }

#include <pthread.h>
#include <unistd.h>

int c;
int d;
struct A
{
  A() { ++c; }
  ~A() {
    if (++d == 3)
      _exit (0);
  }
};

thread_local A a;

void *thread_main(void *)
{
  A* ap = &a;
}

int main()
{
  pthread_t thread;
  thread_main(0);
  pthread_create (&thread, 0, thread_main, 0);
  pthread_join(thread, 0);
  pthread_create (&thread, 0, thread_main, 0);
  pthread_join(thread, 0);

  // The dtor for a in the main thread is run after main exits, so we
  // return 1 now and override the return value with _exit above.
  if (c != 3 || d != 2)
    __builtin_abort();
  return 1;
}
