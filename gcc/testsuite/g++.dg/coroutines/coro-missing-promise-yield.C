//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

struct MissingPromiseYield {
  coro::coroutine_handle<> handle;
  MissingPromiseYield () : handle (nullptr) {}
  MissingPromiseYield (coro::coroutine_handle<> handle) : handle (handle) {}
  struct missing_yield {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    MissingPromiseYield get_return_object() {
      return MissingPromiseYield (coro::coroutine_handle<missing_yield>::from_promise (*this));
    }
    void return_value (int v) {}
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<MissingPromiseYield> {
    using promise_type = MissingPromiseYield::missing_yield;
};

MissingPromiseYield
bar ()
{
  co_yield 22; // { dg-error {no member named 'yield_value' in} }
  co_return 0;
}

// check we have not messed up continuation of the compilation.
template <class... Args>
struct void_t_imp {
  using type = void;
};

int main (int ac, char *av[]) {
  MissingPromiseYield x = bar ();
  return 0;
}
