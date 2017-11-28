// { dg-do run }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target tls_runtime }
// { dg-require-effective-target pthread }
// { dg-options -pthread }
// { dg-add-options tls }

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
  return 0;
}

#include <pthread.h>

int main()
{
  pthread_t thread;
  pthread_create (&thread, 0, thread_main, 0);
  pthread_join(thread, 0);
  pthread_create (&thread, 0, thread_main, 0);
  pthread_join(thread, 0);

  if (c != 2 || d != 2)
    __builtin_abort();
}
