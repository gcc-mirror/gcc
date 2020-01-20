//  { dg-do run }

// Check correct operation of co_await in a loop without local state.

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

/* Valued with an await_transform.  */
int gX = 1;

coro1
f ()
{
  for (;;)
    {
      gX += co_await 11;
      if (gX > 100)
        break;
    }
  co_return gX;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();
  PRINT ("main: got coro1 - checking gX");
  if (gX != 1)
    {
      PRINTF ("main: gX is wrong : %d, should be 1\n", gX);
      abort ();
    }
  PRINT ("main: gX OK -- looping");
  do {
    PRINTF ("main: gX : %d \n", gX);
    f_coro.handle.resume();
  } while (!f_coro.handle.done());

  int y = f_coro.handle.promise().get_value();
  // first value above 100 is 10*11 + 1.
  if (y != 111)
    {
      PRINTF ("main: y is wrong : %d, should be 111\n", y);
      abort ();
    }
  puts ("main: done");
  return 0;
}
