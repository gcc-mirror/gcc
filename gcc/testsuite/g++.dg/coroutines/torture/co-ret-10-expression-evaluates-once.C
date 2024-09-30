// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check that "co_return expression;" only evaluates expression once.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* Give foo() a measureable side-effect.  */
int gX = 1;
__attribute__((__noinline__))
int foo (void)
{ 
  PRINT ("called the int fn foo");
  gX += 1;
  return gX;
}

struct coro1
f () noexcept
{
  PRINT ("coro1: about to return");
  co_return foo();
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f ();
  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();
  x.handle.resume();
  // We want to check that foo() was called exactly once.
  if (gX != 2) 
    {
      PRINT ("main: failed check for a single call to foo()");
      abort ();
    }
  PRINT ("main: after resume");
  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
