// Test for cleanups in the main thread without -pthread.

/* wrapping doesn't reflect _exit value */
// { dg-do run { target { c++11 && unwrapped } } }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

extern "C" void _exit (int);

int c;
struct A
{
  A() { ++c; }
  ~A() { if (c == 1) _exit(0); }
};

void f()
{
  thread_local A a;
}

void *thread_main(void *)
{
  f(); f(); f();
}

int main()
{
  thread_main(0);

  // The dtor for a in the main thread is run after main exits, so we
  // return 1 now and override the return value with _exit above.
  return 1;
}
