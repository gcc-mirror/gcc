//  { dg-do run }

// Test that we correctly re-write multiple uses of a function param
// in the body.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f (int x) noexcept
{
  if (x > 30)
    {
      PRINT ("coro1: about to return k");
      co_return 6174;
    }
  else if (x > 20)
    {
      PRINT ("coro1: about to return the answer");
      co_return 42;
    }
  else
    {
      PRINT ("coro1: about to return 0");
      co_return 0;
    }
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f (25);
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
