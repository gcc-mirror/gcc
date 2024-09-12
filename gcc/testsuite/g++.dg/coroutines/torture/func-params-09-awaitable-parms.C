// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

// Check that we correctly handle params with non-trivial DTORs and
// use the correct copy/move CTORs.

#include "../coro.h"
#include <vector>

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

int regular = 0;
int copy = 0;
int move = 0;

/* This is a more sophisticated awaitable... */

struct FooAwaitable {
  FooAwaitable(int _v) : value(_v), x(1, _v)
    {
      regular++;
      PRINTF ("FooAwaitable(%d)\n",_v);
    }

  FooAwaitable(const FooAwaitable& t)
    {
      value = t.value;
      x = t.x;
      copy++;
      PRINTF ("FooAwaitable(&), %d\n",value);
    }

  FooAwaitable(FooAwaitable&& s)
    {
      value = s.value;
      s.value = -1;
      x = std::move(s.x);
      s.x = std::vector<int> ();
      move++;
      PRINTF ("FooAwaitable(&&), %d\n", value);
    }

  ~FooAwaitable() {PRINTF ("~FooAwaitable(), %d\n", value);}

  bool await_ready() { return false; }
  void await_suspend(coro::coroutine_handle<>) {}
  int await_resume() { return value + x[0];}

  void return_value(int _v) { value = _v;}

  int value;
  std::vector<int> x;
};

coro1
my_coro (FooAwaitable t_lv, FooAwaitable& t_ref, FooAwaitable&& t_rv_ref)
{
  PRINT ("my_coro");
  // We are created suspended, so correct operation depends on
  // the parms being copied.
  int sum = co_await t_lv;
  PRINT ("my_coro 1");
  sum += co_await t_ref;
  PRINT ("my_coro 2");
  // This can't work for the rvalue ref, it's always dangling.
  //sum += co_await t_rv_ref;
  //PRINT ("my_coro 3");
  co_return sum;
}

int main ()
{

  PRINT ("main: create coro1");
  FooAwaitable thing (4);
  coro1 x = my_coro (FooAwaitable (1), thing, FooAwaitable (2));
  PRINT ("main: done ramp");

  if (x.handle.done())
    abort();
  x.handle.resume();
  PRINT ("main: after resume (initial suspend)");

  // now do the three co_awaits.
  while(!x.handle.done())
    x.handle.resume();
  PRINT ("main: after resuming 2 co_awaits");

  /* Now we should have the co_returned value.  */
  int y = x.handle.promise().get_value();
  if (y != 10)
    {
      PRINTF ("main: wrong result (%d).", y);
      abort ();
    }

  if (regular != 3 || copy != 0 || move != 1)
    {
      PRINTF ("main: unexpected ctor use (R:%d, C:%d, M:%d)\n",
	      regular, copy, move);
      abort ();
    }

  PRINT ("main: returning");
  return 0;
}
