//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

/* Valued with an await_transform.  */
int gX = 1;
coro1 f ()
{
  gX = co_await 11 + co_await 15;
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
  PRINT ("main: resuming [2] one side of add");
  f_coro.handle.resume();
  PRINT ("main: resuming [3] other side of add");
  f_coro.handle.resume();
  if (gX != 26)
    {
      PRINTF ("main: gX is wrong : %d, should be 26\n", gX);
      abort ();
    }
  /* we should now have returned with the co_return (57) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 57)
    {
      PRINTF ("main: y is wrong : %d, should be 57\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
