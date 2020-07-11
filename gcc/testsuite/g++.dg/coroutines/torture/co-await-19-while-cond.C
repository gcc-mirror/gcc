// { dg-do run }

// Test co-await in while condition.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* An awaiter that suspends always and returns a boolean as the
   await_resume output.  */
struct BoolAwaiter {
  bool v;
  BoolAwaiter (bool _v) : v(_v) {}
  bool await_ready () { return false; }
  void await_suspend (coro::coroutine_handle<>) {}
  bool await_resume () { return v; }
};

//extern bool tt(void);
int three = 3;
struct coro1
my_coro (bool t)
{
  //int three = 3;
  while (co_await BoolAwaiter (t) && t)
    {
      int five = three + 2;
      co_yield 6169 + five;
    }

  co_return 42;
}

int main ()
{
  PRINT ("main: create coro");
  struct coro1 x = my_coro (false);

  if (x.handle.done())
    {
      PRINT ("main: apparently done when we shouldn't be...");
      abort ();
    }

  PRINT ("main: resume initial suspend");
  x.handle.resume();

  // will be false - so no yield expected.
  PRINT ("main: while condition");
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
