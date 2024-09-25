// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// Check co_return co_await 

#include "../coro.h"

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* A very simple overload.  */
struct test 
{
  auto operator co_await() & noexcept { 
    return coro1::suspend_always_intprt{};
  }

  auto operator co_await() && noexcept { 
    return coro1::suspend_always_longprtsq(3L);
  }
};

template<typename RESULT, typename PARAM>
RESULT
f (PARAM thing) noexcept
{
  co_yield co_await static_cast<PARAM&&>(thing);
  co_return 6174;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 x = f<coro1, test> (test{});
  if (x.handle.done())
    abort();

  PRINT ("main: resuming (initial suspend)");
  x.handle.resume();
  PRINT ("main: resuming (await intprt)");
  x.handle.resume();

  int y = x.handle.promise().get_value();
  if ( y != 9 )
    {
      PRINTF ("main: co-yield gave %d, should be 9\n", y);
      abort ();
    }

  PRINT ("main: got coro1 - resuming (co_yield)");
  if (x.handle.done())
    abort();
  x.handle.resume();

  y = x.handle.promise().get_value();
  if ( y != 6174 )
    {
      PRINTF ("main: co-return gave %d, should be 9\n", y);
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
