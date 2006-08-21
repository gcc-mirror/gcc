// Test for "sticky cancel": if a catch (...) block discards the
// cancellation exception, a new one is raised at the next cancellation
// point.

// This test only applies to glibc targets.
// { dg-do run { target *-*-linux* } }
// { dg-options "-pthread" }

#include <pthread.h>
#include <cxxabi.h>
extern "C" int printf (const char *, ...);

void* thread_main(void*)
{
  try
    {
      // Spin until we get cancelled.
      while (1)
	pthread_testcancel();
    }
  catch (...)
    {
      // Catch and discard the forced unwind.
      printf ("caught ...\n");
    }

  try
    {
      // Start unwinding again.
      pthread_testcancel();
    }
  catch (...)
    {
      // Catch and discard again.  This time the thread exits before the
      // next cancellation point, so we're done.
      printf ("caught ... again\n");
      return 0;
    }

  return (void*)4;
}

int main()
{
  pthread_t thread;
  int r;
  void *p;

  r = pthread_create (&thread, NULL, thread_main, NULL);
  if (r)
    return 1;

  r = pthread_cancel (thread);
  if (r)
    return 2;

  r = pthread_join (thread, &p);
  if (r)
    return 3;

  return (int)p;
}
