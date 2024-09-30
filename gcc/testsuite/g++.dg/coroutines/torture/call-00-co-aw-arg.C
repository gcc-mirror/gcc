// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check that we can use co_await as a call parm.

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

int gX = 1;

__attribute__((__noinline__))
static int
foo (int x)
{
  return x + 2;
}

/* Function with a single await.  */
coro1 
f ()
{
  gX = foo (co_await 9);
  co_return gX + 31;
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

  PRINT ("main: resuming [1] (initial suspend)");
  f_coro.handle.resume();
  PRINT ("main: resuming [2] (await 9 parm)");
  f_coro.handle.resume();

  if (gX != 11)
    {
      PRINTF ("main: gX is wrong : %d, should be 11\n", gX);
      abort ();
    }

  /* we should now have returned with the co_return 11 + 31) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done'");
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
