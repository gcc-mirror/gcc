//  { dg-do run }

// Test that we can use a function param in a co_xxxx status.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int main ()
{
  int val;

  auto f = [&] (int x) -> coro1
  {
    if (val + x > 25)
      {
        PRINT ("coro1: about to return k");
        co_return 6174;
      }
    else if (val + x > 20)
      {
        PRINTF ("coro1: about to co-return %d\n", val + x);
        co_return val + x;
      }
    else if (val + x > 5)
      {
        PRINTF ("coro1: about to co-return %d\n", val);
        co_return val;
      }
    else
      {
        PRINT ("coro1: about to return 0");
        co_return 0;
      }
  };

  PRINT ("main: create coro1");

  val = 20;  // We should get this by ref.
  int arg = 5; // and this as a regular parm.

  coro1 x = f (arg);
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != 25 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
