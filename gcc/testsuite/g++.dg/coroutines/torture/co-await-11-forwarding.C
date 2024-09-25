//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Test of forwarding a templated awaitable to co_await.

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* Valued with an await_transform.  */

template< typename AWAITABLE >
coro1
test_fwd (AWAITABLE&& awaitable)
{
  // the await_resume() just returns the saved int value.
  int a = co_await std::forward<AWAITABLE>(awaitable);
  // Which we co-return to the promise so that it can be
  // retrieved.
  co_return a;
}

int main ()
{
  // We have an awaitable that stores the int it was constructed with.
  coro1::suspend_always_intprt g(15);
  struct coro1 g_coro = test_fwd (g);

  PRINT ("main: resuming g [1] (initial suspend)");
  g_coro.handle.resume();

  PRINT ("main: resuming g [2] co_await");
  g_coro.handle.resume();

  int y = g_coro.handle.promise().get_value();
  if (y != 15)
    {
      PRINTF ("main: y is wrong : %d, should be 15\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
