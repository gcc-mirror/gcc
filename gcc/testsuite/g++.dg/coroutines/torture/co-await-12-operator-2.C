//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Basic check of the co_await operator overload.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* A very simple overload.  */
struct empty 
{
  auto operator co_await() & noexcept { 
    return coro1::suspend_always_intprt{};
  }
  auto operator co_await() && noexcept { 
    return coro1::suspend_always_longprtsq(3L);
  }
};

empty e{};

coro1
f ()
{
  int a = co_await e; /* operator ovl lv. */
  int b = co_await empty{}; /* operator ovl rv. */
  co_return b + a;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = f ();

  if (f_coro.handle.done())
    {
      PRINT ("main: we should not be 'done'");
      abort ();
    }

  PRINT ("main: resuming [1] initial suspend");
  f_coro.handle.resume();

  PRINT ("main: resuming [2] co_await a");
  f_coro.handle.resume();

  PRINT ("main: resuming [3] co_await b");
  f_coro.handle.resume();

  /* we should now have returned with the co_return (14) */
  if (!f_coro.handle.done())
    {
      PRINT ("main: we should be 'done' ");
      abort ();
    }

  int y = f_coro.handle.promise().get_value();
  if (y != 14)
    {
      PRINTF ("main: y is wrong : %d, should be 14\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
