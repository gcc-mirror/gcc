//  { dg-do run }
//  { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

// Test the case where the awaitables are local vars, and therefore already
// have a frame representation - and should not be copied to a second frame
// entry (since elision of that copy would break the assumptions made in the
// management of the lifetime of the awaitable).

#include "../coro.h"
#include <vector>

// boiler-plate for tests of codegen
#include "../coro1-ret-int-yield-int.h"

/* Make a non-trivial awaitable.  */
struct Awaitable
{
  int v;
  std::vector<int> x;
  Awaitable () : v(0), x(1,0) {PRINTF ("Awaitable()\n");} 
  Awaitable (int _v) : v(_v), x(1,_v) {PRINTF ("Awaitable(%d)\n",_v);}

  bool await_ready () { return false; }
  void await_suspend(coro::coroutine_handle<>) {}
  int await_resume() { return v + x[0];}

  ~Awaitable () {PRINTF ("~Awaitable(%d)\n",v);}
};

coro1
my_coro (int start) noexcept
{
  PRINT ("my_coro");
  Awaitable aw0 = Awaitable (start);
  Awaitable aw1 = Awaitable (4);
  Awaitable aw2 = Awaitable (10);

  int sum;
  /* We are started with a suspend_always init suspend expr.  */
  sum = co_await aw0;
  PRINT ("my_coro 1");
  sum += co_await aw1;
  PRINT ("my_coro 2");
  sum += co_await aw2;
  PRINT ("my_coro 3");

  co_return sum;
}

int main ()
{
  PRINT ("main: create my_coro");
  struct coro1 x = my_coro (7);
  PRINT ("main: ramp done, resuming init suspend");
  if (x.handle.done())
    abort();
  x.handle.resume();

  // now do the three co_awaits.
  while(!x.handle.done())
    x.handle.resume();
  PRINT ("main: after resuming 3 co_awaits");

  /* Now we should have the co_returned value.  */
  int y = x.handle.promise().get_value();
  if (y != 42)
    {
      PRINTF ("main: wrong result (%d).", y);
      abort ();
    }

  PRINT ("main: returning");
  return 0;
}
