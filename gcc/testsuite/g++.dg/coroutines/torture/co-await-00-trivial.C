//  { dg-do run }

// The simplest co_await we can do.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int gX = 1;

coro1
f ()
{
  co_await coro1::suspend_always_prt{};
  co_return gX + 10;
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
  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done' [1]");
      abort ();
    }
  PRINT ("main: resuming [1] initial suspend");
  f_coro.handle.resume();
  PRINT ("main: resuming [2] co_await");
  f_coro.handle.resume();
  /* we should now have returned with the co_return (15) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 11)
    {
      PRINTF ("main: y is wrong : %d, should be 11\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
