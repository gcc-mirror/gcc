// { dg-do run }

// template parm in a class

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

template <typename T>
class foo
{
  public:
  coro1 meth (T y)
    {
      PRINT ("coro1: about to return");
      T x = y;
      co_return co_await x + 3;
    }
};

int main ()
{
  foo<int> inst {};
  PRINT ("main: create coro1");
  coro1 x = inst.meth (17);
  if (x.handle.done())
    abort();

  x.handle.resume();
  PRINT ("main: after resume (initial suspend)");

  x.handle.resume();
  PRINT ("main: after resume (co_await)");

  /* Now we should have the co_returned value.  */
  int y = x.handle.promise().get_value();
  if ( y != 20 )
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
