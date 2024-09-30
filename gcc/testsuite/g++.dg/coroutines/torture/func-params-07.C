// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test that we copy simple parms correctly by value, reference or
// rvalue reference.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-refs-and-ctors.h"

coro1
my_coro (int v1, int& v2, int&& v3)
{
  co_yield v1 + v2 + v3;
  co_return v1 + v2 + v3;
}

int main ()
{
  PRINT ("main: create coro1");
  int lv = 1;
  int lvr = 2;
  coro1 x = my_coro (lv, lvr, lvr+2);

  if (x.handle.done())
    abort();

  x.handle.resume();
  PRINT ("main: after resume (initial suspend)");

  /* Now we should have the co_yielded value.  */
  int y = x.handle.promise().get_value();
  if ( y != 7 )
    {
      PRINTF ("main: wrong result (%d).", y);
      abort ();
    }

  /* So we should be suspended at the yield, now change the
     values so that we can determine that the reference persists
     and the copy was made correctly.  */
  lv = 5; // should be ignored
  lvr = 3; // should be enacted

  x.handle.resume();
  PRINT ("main: after resume (yield)");

  /* Now we should have the co_returned value.  */
  y = x.handle.promise().get_value();
  if ( y != 8 )
    {
      PRINTF ("main: wrong result (%d).", y);
      abort ();
    }

  y = x.handle.promise().get_v2();
  if ( y != 2 )
    {
      PRINTF ("main: wrong result 2 (%d).", y);
      abort ();
    }

  y = x.handle.promise().get_v3();
  if ( y != 4 )
    {
      PRINTF ("main: wrong result 3 (%d).", y);
      abort ();
    }

  if (!x.handle.done())
    {
      PRINT ("main: apparently not done...");
      abort ();
    }

  x.handle.destroy();
  x.handle = NULL;

  PRINT ("main: returning");
  return 0;
}
