//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check cascaded co_await operations.

#include "../coro.h"

#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

/* Valued with an await_transform.  */
int gX = 1;
coro1 f ()
{
  /* We are going to use an await transform that takes a long, the
     await_resume squares it.
     so we get 11 ** 4, 14641.  */
  gX = (int) co_await co_await 11L;
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
  PRINT ("main: resuming [1] - inital suspend");
  f_coro.handle.resume();

  PRINT ("main: resuming [2] - nested");
  f_coro.handle.resume();
  PRINT ("main: resuming [3] - outer");
  f_coro.handle.resume();

  if (gX != 14641)
    {
      PRINTF ("main: gX is wrong : %d, should be 14641\n", gX);
      abort ();
    }
  /* we should now have returned with the co_return (14672) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  int y = f_coro.handle.promise().get_value();
  if (y != 14672)
    {
      PRINTF ("main: y is wrong : %d, should be 14672\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
