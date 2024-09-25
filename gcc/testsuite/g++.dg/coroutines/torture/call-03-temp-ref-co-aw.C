// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check  foo (compiler temp, co_await).

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

int gX = 1;

__attribute__((__noinline__))
static int
bar (int x, const int& y)
{
  return x + y;
}

/* Function with a compiler temporary and a co_await.  */
coro1
g ()
{
  gX = bar (gX + 8, co_await 2);
  co_return gX + 31;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 g_coro = g ();
  PRINT ("main: got coro1 - checking gX");
  if (gX != 1)
    {
      PRINTF ("main: gX is wrong : %d, should be 1\n", gX);
      abort ();
    }
  if (g_coro.handle.done())
    {
      PRINT ("main: we should not be 'done' [1]");
      abort ();
    }

  PRINT ("main: resuming [1] (initial suspend)");
  g_coro.handle.resume();

  PRINT ("main: resuming [2] (parm 1)");
  g_coro.handle.resume();

  if (gX != 11)
    {
      PRINTF ("main: gX is wrong : %d, should be 11\n", gX);
      abort ();
    }

  /* we should now have returned with the co_return 11 + 31) */
  if (!g_coro.handle.done())
    {
      PRINT ("main: we should be 'done'");
      abort ();
    }

  int y = g_coro.handle.promise().get_value();
  if (y != 42)
    {
      PRINTF ("main: y is wrong : %d, should be 42\n", y);
      abort ();
    }

  PRINT ("main: done");
  return 0;
}