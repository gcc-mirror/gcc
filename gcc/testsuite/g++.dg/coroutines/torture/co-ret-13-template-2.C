// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check type dependent function parms. 

#include "../coro.h"

// boiler-plate for tests of codegen
// there is a promise ctor that takes a single int.
#include "../coro1-ret-int-yield-int.h"

template <typename T>
coro1
f (T y) noexcept
{
  PRINT ("coro1: about to return");
  __attribute__((__unused__)) T x = y;
  co_return 3;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f<int>(17);

  /* We should have created the promise with an initial value of
     17.  */
  int y = x.handle.promise().get_value();
  if ( y != 17 )
    {
      PRINT ("main: wrong promise init.");
      abort ();
    }

  PRINT ("main: got coro1 - resuming");
  if (x.handle.done())
    abort();

  x.handle.resume();
  PRINT ("main: after resume (initial suspend)");

  /* Now we should have the co_returned value.  */
  y = x.handle.promise().get_value();
  if ( y != 3 )
    {
      PRINT ("main: wrong answer.");
      abort ();
    }

  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }
  PRINT ("main: returning");
  return 0;
}
