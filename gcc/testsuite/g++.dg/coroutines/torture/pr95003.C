// { dg-do run }

#include "../coro.h"
#include "../coro1-ret-int-yield-int.h"

// This tests that, when we insert bind scopes to contain variables that
// have been promoted from compiler temporaries to extend their lifetimes
// to a containing full expression, the inserted bind scopes have their
// tree-side-effects set.

struct Awaitable {
  int v;
  Awaitable (int _v) : v(_v) {}
  bool await_ready() { return false; }
  void await_suspend(std::coroutine_handle<coro1::promise_type>) {}
  int await_resume() { return v; }
  auto operator co_await() { return *this; }
};

coro1
my_coro
(int x)
{
  int sum = 0;
  for (unsigned i = 0; i < 100; ++i) {
    sum += co_await Awaitable{x+1};
  }
  co_return sum;
}

int main ()
{
  PRINT ("main: create coro1");
  struct coro1 f_coro = my_coro (0);

  PRINT ("main: OK -- looping");

  do {
    f_coro.handle.resume();
  } while (!f_coro.handle.done());

  int y = f_coro.handle.promise().get_value();
  if (y != 100)
    {
      PRINTF ("main: y is wrong : %d, should be 100\n", y);
      abort ();
    }
  PRINT ("main: done");
  return 0;
}
