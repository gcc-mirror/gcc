//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

namespace coro = std::experimental::coroutines_n4775;

/* Diagose missing return_void() in the promise type.  */
struct DummyYield {
  coro::coroutine_handle<> handle;
  DummyYield (coro::coroutine_handle<> handle) : handle (handle) {}
  struct dummy_yield {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    DummyYield get_return_object() {
      return DummyYield (coro::coroutine_handle<dummy_yield>::from_promise (*this));
    }
    void yield_value (int v) {}
    void return_value (int v) {}
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<DummyYield> {
    using promise_type = DummyYield::dummy_yield;
};

DummyYield bar () {
  co_yield; // { dg-error {expected primary-expression before} }
  co_return 0;
}

int main (int ac, char *av[]) {
  DummyYield x = bar ();
  return 0;
}
