// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check co_return co_await 

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return co_await coro1::suspend_always_intprt{};
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
  PRINT ("main: after resume 2 (await intprt)");

  int y = x.handle.promise().get_value();
  if ( y != 5 )
    abort ();
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
