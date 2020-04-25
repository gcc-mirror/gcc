//  { dg-do run }

// Test returning an int.
// We will use the promise to contain this to avoid having to include
// additional C++ headers.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f (int v)
{
  if (co_await coro1::suspend_always_intprt{v} == 10)
    co_return 6174;
  else
    {
      int v = 42;
      PRINT ("coro1: about to return");
      co_return v;
    }
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f (10);
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  // initial susp
  x.handle.resume();
  PRINT ("main: after resume");
  // if condition
  x.handle.resume();
  int y = x.handle.promise().get_value();
  if ( y != 6174 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
