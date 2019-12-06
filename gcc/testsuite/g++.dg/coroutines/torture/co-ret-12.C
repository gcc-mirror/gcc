// { dg-do run }

// Check co_return function (co_await)

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

__attribute__((__noinline__))
static int
foo (int x)
{
  return x + 2;
}

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return foo (co_await 5);
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();

  x.handle.resume();
  PRINT ("main: after resume 1 (initial suspend)");
  x.handle.resume();
  PRINT ("main: after resume 2 (await parm)");

  int y = x.handle.promise().get_value();
  if ( y != 7 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
