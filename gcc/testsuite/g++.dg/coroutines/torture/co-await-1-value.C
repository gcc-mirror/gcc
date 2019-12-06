//  { dg-do run }

/* The simplest valued co_await we can do.  */

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int gX = 1;

coro1
f ()
{
  gX = co_await coro1::suspend_always_intprt{};
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
  PRINT ("main: resuming [2] co_await suspend_always_intprt");
  f_coro.handle.resume();
  if (gX != 5)
    {
      PRINTF ("main: gX is wrong : %d, should be 5\n", gX);
      abort ();
    }
  /* we should now have returned with the co_return (15) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 15)
    {
      PRINTF ("main: y is wrong : %d, should be 15\n", y);
      abort ();
    }
  puts ("main: done");
  return 0;
}
