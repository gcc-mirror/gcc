//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test local vars in nested scopes

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f (int x) noexcept
{
  int y = x;
  const int test = 20;
  if (y > test)
    {
      int fred = y - 20;
      PRINTF ("coro1: about to return %d\n", fred);
      co_return fred;
    }
  else
    {
      PRINT ("coro1: about to return the answer\n");
      co_return y;
    }

  co_return x;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f (6194);
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume");
  int y = x.handle.promise().get_value();
  if ( y != 6174 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
      //x.handle.resume();
    }
  PRINT ("main: returning");
  return 0;
}
