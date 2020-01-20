//  { dg-do run }

// Test promise construction from function args list.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f (int x) noexcept
{
  PRINT ("coro1: about to return");
  co_return 42;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f (555);
  int y = x.handle.promise().get_value();
  if ( y != 555 )
    {
      PRINT ("main: incorrect ctor value");
      abort ();
    }
  PRINTF ("main: after coro1 ctor %d - now resuming\n", y);
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  y = x.handle.promise().get_value();
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
