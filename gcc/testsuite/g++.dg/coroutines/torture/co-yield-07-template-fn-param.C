// { dg-do run }

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
  T x = y;
  co_yield x + 3;
  co_return 42;
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
      PRINTF ("main: wrong promise init (%d).", y);
      abort ();
    }
  if (x.handle.done())
    abort();

  PRINT ("main: got coro1 - resuming");
  x.handle.resume();
  PRINT ("main: after resume (initial suspend)");

  if (x.handle.done())
    abort();

  /* Now we should have the co_yielded value.  */
  y = x.handle.promise().get_value();
  if ( y != 20 )
    {
      PRINTF ("main: wrong result (%d).", y);
      abort ();
    }

  PRINT ("main: after resume (co_yield)");
  x.handle.resume();

  /* now we should have the co_returned value.  */
  y = x.handle.promise().get_value();
  if ( y != 42 )
    {
      PRINTF ("main: wrong result (%d).", y);
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
