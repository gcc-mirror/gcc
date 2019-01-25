//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

namespace coro = std::experimental::coroutines_n4775;

/* Diagose missing return_void() in the promise type.  */
struct MissingRetValue {
  coro::coroutine_handle<> handle;
  MissingRetValue (coro::coroutine_handle<> handle) : handle (handle) {}
  struct missing_retvoid {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    MissingRetValue get_return_object() {
      return MissingRetValue (coro::coroutine_handle<missing_retvoid>::from_promise (*this));
    }
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<MissingRetValue> {
    using promise_type = MissingRetValue::missing_retvoid;
};

MissingRetValue bar () {
  co_return 6174; // { dg-error {no member named 'return_value' in} }
}

int main (int ac, char *av[]) {
  MissingRetValue x = bar ();
  return 0;
}
