// { dg-do run }

// Test co-await in while condition.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* An awaiter that suspends always and returns an int as the
   await_resume output.  */
struct IntAwaiter {
  int v;
  IntAwaiter (int _v) : v(_v) {}
  bool await_ready () { return false; }
  void await_suspend (coro::coroutine_handle<>) {}
  int await_resume () { return v; }
};

struct coro1
my_coro (int t)
{
  int v = 6174;
  switch (co_await IntAwaiter (t) + 5)
  {
    default: break;
    case 22: co_return v;
  }
  co_return 42;
}

int main ()
{
  PRINT ("main: create coro");
  struct coro1 x = my_coro (17);

  if (x.handle.done())
    {
      PRINT ("main: apparently done when we should not be...");
      abort ();
    }

  PRINT ("main: resume initial suspend");
  x.handle.resume();

  PRINT ("main: resume switch condition");
  x.handle.resume();

  int y = x.handle.promise().get_value();
  if ( y != 6174 )
    {
      PRINTF ("main: apparently wrong value : %d\n", y);
      abort ();
    }

  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
