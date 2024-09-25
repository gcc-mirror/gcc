// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check type dependent function parms. 

#include "../coro.h"

// boiler-plate for tests of codegen
// there is a promise ctor that takes a single int.
#include "../coro1-ret-int-yield-int.h"

template <typename T, typename U, typename V>
coro1
f (T x, U y, V z) noexcept
{
  PRINT ("coro1: about to return");
  T xi = (T) y;
  T yi = (T) z;
  T zi = x;
  co_return 3 + xi + yi + zi;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f<int, float, double>(2, 18.0F, 19.0);

  /* We should be using the default promise ctor, which sets the value
     to -1.  */
  int y = x.handle.promise().get_value();
  if ( y != -1 )
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
  if ( y != 42 )
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
