// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

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

/* An awaiter that suspends always and returns a boolean as the
   await_resume output.  The boolean toggles on each call.  */

struct BoolAwaiter {
  bool v;
  BoolAwaiter (bool _v) : v(_v) {}
  bool await_ready () { return false; }
  void await_suspend (coro::coroutine_handle<>) {}
  bool await_resume () { v = !v; return v; }
};

/* We will be able to establish that the second part of the conditional
   expression is not evaluated (if it was, then we'd need an additional
   resume to complete the coroutine).  */

struct coro1
my_coro (int t)
{

  bool x = co_await IntAwaiter (t) == 5 || co_await BoolAwaiter (false);

  if (x)
    co_return 6174;
  co_return 42;
}

int main ()
{
  PRINT ("main: create coro");
  struct coro1 x = my_coro (5);

  if (x.handle.done())
    {
      PRINT ("main: apparently done when we should not be...");
      abort ();
    }

  PRINT ("main: resume initial suspend");
  x.handle.resume();

  PRINT ("main: resume IntAwaiter");
  x.handle.resume();

  // The evaluation of 'co_await IntAwaiter (t) == 5' should be true, thus
  // the second co_await in the expression will be unexecuted. 
  
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
