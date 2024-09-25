// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check co_return co_await 

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

/* Function with a single await.  */
struct coro1
f () noexcept
{
  PRINT ("f: about to yield");
  co_yield foo (co_await 40);

  PRINT ("f: about to return 6174");
  co_return 6174;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  if (x.handle.done())
    abort();

  PRINT ("main: resuming (initial suspend)");
  x.handle.resume();
  PRINT ("main: resuming (await intprt)");
  x.handle.resume();

  PRINT ("main: after resume (2)");
  int y = x.handle.promise().get_value();
  if ( y != 42 )
    abort ();
  PRINT ("main: apparently got 42");

  PRINT ("main: got coro1 - resuming (co_yield)");
  if (x.handle.done())
    abort();
  x.handle.resume();

  PRINT ("main: after resume (co_yield)");
  y = x.handle.promise().get_value();
  if ( y != 6174 )
    abort ();
  PRINT ("main: apparently got 6174");
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
