//  { dg-do run }

// Show that we are correctly accessing class variables.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

class Foo
{
  int v;
  public:
  Foo () : v(0) {};
  Foo (int x) : v(x) {};
  coro1 meth ()
    {
      PRINT ("coro1: about to return");
      co_return v++;
    }
};

int main ()
{
  Foo inst (42);
  int y;
  {
    PRINT ("main: create coro1 [instance 1]");
    coro1 x = inst.meth ();
    if (x.handle.done())
      abort();
    PRINT ("main: got coro1 - resuming (initial suspend)");
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
  }
  PRINT ("main: create coro1 [instance 2]");
  coro1 p = inst.meth ();
  if (p.handle.done())
    abort();
  PRINT ("main: got coro1 - resuming (initial suspend)");
  p.handle.resume();
  PRINT ("main: after resume");
  y = p.handle.promise().get_value();
  if ( y != 43 )
    abort ();
  if (!p.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
