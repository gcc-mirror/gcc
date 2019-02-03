//  { dg-additional-options "-fsyntax-only -w" }
#include "coro.h"

namespace coro = std::experimental::coroutines_n4775;

/* Diagose missing return_void() in the promise type.  */
struct MissingGRO {
  coro::coroutine_handle<> handle;
  MissingGRO (coro::coroutine_handle<> handle) : handle (handle) {}
  struct missing_gro {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    void return_void () {}
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<MissingGRO> {
    using promise_type = MissingGRO::missing_gro;
};

MissingGRO bar () { // { dg-error {no member named 'get_return_object' in} }
  co_return;
}

int main (int ac, char *av[]) {
  MissingGRO x = bar ();
  return 0;
}
