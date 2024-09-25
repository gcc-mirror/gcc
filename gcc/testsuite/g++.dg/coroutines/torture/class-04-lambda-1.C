// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

// template parm in a class

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

template <typename T>
class foo
{
  public:
  auto get_lam ()
    {
      auto l = [](T y) -> coro1
      {
	T x = y;
	co_return co_await x + 3;
      };
      return l;
    }
};

int main ()
{
  foo<int> inst {};
  auto ll = inst.get_lam ();

  PRINT ("main: create coro1");
  int arg = 17; // avoid a dangling reference
  coro1 x = ll (arg);
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
