//  { dg-do run }

/* The simplest valued co_await we can do.  */

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"


coro1
f ()
{
  int t1 = 5;
  int t2 = 5;
  auto gX = co_await coro1::suspend_always_intrefprt{t1};
  if (gX != t1)
	  abort();
  decltype(auto) gX1 = co_await coro1::suspend_always_intrefprt{t2};
  if (&gX1 != &t2)
	  abort();
  co_return t1 + 10;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();
  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done' [1]");
      abort ();
    }
  PRINT ("main: resuming [1] initial suspend");
  while (!f_coro.handle.done())
    f_coro.handle.resume();
  /* we should now have returned with the co_return (15) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
