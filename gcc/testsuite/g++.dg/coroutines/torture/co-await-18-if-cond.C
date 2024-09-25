// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test co-await in if condition.

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

/* An awaiter that suspends always and returns an int as the
   await_resume output.  */
struct IntAwaiter {
  int v;
  IntAwaiter (int _v) : v(_v) {}
  bool await_ready () { return false; }
  void await_suspend (coro::coroutine_handle<>) {}
  int await_resume () { return v; }
};

//extern int tt ();
  int three = 3;
  int two = 2;

struct coro1
my_coro (bool t) noexcept
{
//  if (int two = 2;tt () || co_await BoolAwaiter (t) && t && co_await IntAwaiter (5) == co_await IntAwaiter (7))
  if (co_await BoolAwaiter (t) && co_await IntAwaiter (5) == 5)
    {
      int five = three + two;
      co_return 6169 + five;
    }
  else
    {
      int five = three + two;
      co_return 37 + five;
    }
}

int main ()
{
  PRINT ("main: create coro");
  struct coro1 x = my_coro (true);

  if (x.handle.done())
    {
      PRINT ("main: apparently done when we should not be...");
      abort ();
    }

  PRINT ("main: resume initial suspend");
  x.handle.resume();

  PRINT ("main: if condition 1 - true");
  x.handle.resume();

  PRINT ("main: if condition 2 - true");
  x.handle.resume();

  PRINT ("main: after resume");
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
