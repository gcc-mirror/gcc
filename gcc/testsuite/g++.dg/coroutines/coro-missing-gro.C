//  { dg-additional-options "-fsyntax-only -w" }

// Diagose missing get_return_object() in the promise type.

#include "coro.h"

struct MissingGRO {
  coro::coroutine_handle<> handle;
  MissingGRO () : handle (nullptr) {}
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

MissingGRO
bar () // { dg-error {no member named 'get_return_object' in} }
{ 
  co_return;
}

int main (int ac, char *av[]) {
  MissingGRO x = bar ();
  return 0;
}
