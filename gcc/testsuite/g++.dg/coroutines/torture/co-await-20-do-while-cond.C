// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test co-await in do-while conditional

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* An awaiter that suspends always and returns a boolean as the
   await_resume output.  The boolean toggles on each call.  */
struct BoolAwaiter {
  bool v;
  BoolAwaiter (bool _v) : v(_v) {}
  bool await_ready () { return false; }
  void await_suspend (coro::coroutine_handle<>) {}
  bool await_resume () { v = !v; return v; }
};

int v = 32;

struct coro1
my_coro (bool t)
{
  auto aw = BoolAwaiter (t);
  do {
    int five = 5;
    v += five;
  } while (co_await aw && !t);

  co_return v;
}

int main ()
{
  PRINT ("main: create coro");
  struct coro1 x = my_coro (false);

  if (x.handle.done())
    {
      PRINT ("main: apparently done when we should not be...");
      abort ();
    }

  PRINT ("main: resume initial suspend");
  x.handle.resume();

  PRINT ("main: resume while test, succeed first time");
  x.handle.resume();

  PRINT ("main: resume while test, fail second");
  x.handle.resume();

  int y = x.handle.promise().get_value();
  if ( y != 42 )
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
