// Test for cleanups with pthread_cancel.

// { dg-do run }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target tls_runtime }
// { dg-require-effective-target pthread }
// { dg-require-effective-target non_bionic }
// { dg-options -pthread }
// { dg-add-options tls }

#include <pthread.h>
#include <unistd.h>

int c;
int d;
struct A
{
  A() { ++c; }
  ~A() { ++d; }
};

void f()
{
  thread_local A a;
}

void *thread_main(void *)
{
  f(); f(); f();
  while (true)
    {
      pthread_testcancel();
      sleep (1);
    }
}

int main()
{
  pthread_t thread;
  pthread_create (&thread, 0, thread_main, 0);
  pthread_cancel(thread);
  pthread_join(thread, 0);
  pthread_create (&thread, 0, thread_main, 0);
  pthread_cancel(thread);
  pthread_join(thread, 0);

   if (c != 2 || d != 2)
     __builtin_abort();
}
