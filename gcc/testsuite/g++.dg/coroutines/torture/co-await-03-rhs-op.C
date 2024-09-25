//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Basic check of co_await with an expression to await transform.

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

int gX = 1;

coro1
f ()
{
  gX = co_await 11 + 15;
  co_return gX + 16;
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
  PRINT ("main: resuming [1] await");
  f_coro.handle.resume();
  if (gX != 26)
    {
      PRINTF ("main: gX is wrong : %d, should be 26\n", gX);
      abort ();
    }
  /* we should now have returned with the co_return (26+16) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 42)
    {
      PRINTF ("main: y is wrong : %d, should be 42\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
