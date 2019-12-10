//  { dg-do run }

// Simplest class.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

class foo
{
  public:
  coro1 meth ()
    {
      PRINT ("coro1: about to return");
      co_return 42;
    }
};

int main ()
{
  foo inst;

  PRINT ("main: create coro1");
  coro1 x = inst.meth ();
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
