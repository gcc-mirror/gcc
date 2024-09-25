//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test co_yield in a loop with no local state.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int gX = 1;

struct coro1
f () noexcept
{
  for (gX = 5; gX < 10 ; gX++)
    {
      PRINTF ("f: about to yield %d\n", gX);
      co_yield gX;
     }

  PRINT ("f: about to return 6174");
  co_return 6174;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();
  PRINT ("main: got coro1 - resuming (1)");
  if (gX != 1)
    {
      PRINTF ("main: gX is wrong : %d, should be 1\n", gX);
      abort ();
    }
  if (f_coro.handle.done())
    abort();
  f_coro.handle.resume();
  PRINT ("main: after resume (1)");
  int y = f_coro.handle.promise().get_value();
  if (y != 5)
    {
      PRINTF ("main: got %d not 5.\n",y);
      abort ();
    }
  PRINT ("main: gX OK -- looping");
  do {
    y = f_coro.handle.promise().get_value();
    if (y != gX)
      {
        PRINTF ("main: got %d not %d.\n",y, gX);
        abort ();
      }
    PRINTF ("main: gX : %d \n", gX);
    f_coro.handle.resume();
  } while (!f_coro.handle.done());

  y = f_coro.handle.promise().get_value();
  if ( y != 6174 )
    abort ();
  PRINT ("main: apparently got 6174");
  if (!f_coro.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
