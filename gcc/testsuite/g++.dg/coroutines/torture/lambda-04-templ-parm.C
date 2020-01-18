// { dg-do run }
// { dg-additional-options "-std=c++2a" }

// generic Lambda with template parm (from c++20)

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

int main ()
{
  auto f = []<typename T>(T y) -> coro1
  {
    PRINT ("coro1: about to return");
    T x = y;
    co_return co_await x + 3;
  };

  PRINT ("main: create coro1");
  coro1 x = f.operator()<int>(17);
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
