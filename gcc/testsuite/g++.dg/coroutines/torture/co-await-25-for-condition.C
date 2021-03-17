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
coro_a (bool t)
{
  int accum = 0;
  for (int x = 3; x < co_await IntAwaiter (10); x++)
    accum += x;

  co_return accum;
}

/* An awaiter that suspends always and returns an int as the
   await_resume output.  */
struct TenAwaiter {
  int v;
  TenAwaiter (int _v) : v(_v) {}
  bool await_ready () { return false; }
  void await_suspend (coro::coroutine_handle<>) {}
  bool await_resume () { return v < 10; }
};

struct coro1
coro_b (bool t)
{
  int accum = 0;
  for (int x = 3; co_await TenAwaiter (x); x++)
    accum += x;

  co_return accum;
}

void
check_a_coro (struct coro1& x)
{
  if (x.handle.done())
    {
      PRINT ("check_a_coro: apparently done when we shouldn't be...");
      abort ();
    }

  PRINT ("check_a_coro: resume initial suspend");
  x.handle.resume();

  // will be false - so no yield expected.
  PRINT ("check_a_coro: resume loops");
  while (!x.handle.done())
    x.handle.resume();

  int y = x.handle.promise().get_value();
  if ( y != 42 )
    {
      PRINTF ("check_a_coro: apparently wrong value : %d\n", y);
      abort ();
    }

  if (!x.handle.done())
    {
      PRINT ("check_a_coro: apparently not done...");
      abort ();
    }
}

int main ()
{
  {
    struct coro1 x = coro_a (false);
    check_a_coro (x);
  }
  {
    struct coro1 x = coro_b (false);
    check_a_coro (x);
  }

  PRINT ("main: returning");
  return 0;
}
