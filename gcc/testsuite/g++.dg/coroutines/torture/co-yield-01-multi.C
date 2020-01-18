//  { dg-do run }

// Test yielding an int.
// We will use the promise to contain this to avoid having to include
// additional C++ headers.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f () noexcept
{
  PRINT ("f: about to yield 42");
  co_yield 42;

  PRINT ("f: about to yield 11");
  co_yield 11;

  PRINT ("f: about to return 6174");
  co_return 6174;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  PRINT ("main: got coro1 - resuming (1)");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume (1)");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    abort ();
  PRINT ("main: apparently got 42 - resuming (2)");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume (2)");
  y = x.handle.promise().get_value();
  if ( y != 11 )
    abort ();
  PRINT ("main: apparently got 11 - resuming (3)");
  if (x.handle.done())
    {
   PRINT ("main: done?");
   abort();
    }
  x.handle.resume();
  PRINT ("main: after resume (2) checking return");
  y = x.handle.promise().get_value();
  if ( y != 6174 )
    abort ();
  PRINT ("main: apparently got 6174");
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
