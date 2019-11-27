//  { dg-additional-options "-fsyntax-only -w" }

#include "coro.h"

// Diagose missing return_void() in the promise type.

struct MissingRetVoid {
  coro::coroutine_handle<> handle;
  MissingRetVoid () : handle (nullptr) {}
  MissingRetVoid (coro::coroutine_handle<> handle) : handle (handle) {}
  struct missing_retvoid {
    coro::suspend_never initial_suspend() { return {}; }
    coro::suspend_never final_suspend() { return {}; }
    MissingRetVoid get_return_object() {
      return MissingRetVoid (coro::coroutine_handle<missing_retvoid>::from_promise (*this));
    }
    void unhandled_exception() { /*std::terminate();*/ };
  };
};

template<> struct coro::coroutine_traits<MissingRetVoid> {
    using promise_type = MissingRetVoid::missing_retvoid;
};

MissingRetVoid
bar ()
{
  co_return; // { dg-error "no member named .return_void. in" }
}

int main (int ac, char *av[]) {
  MissingRetVoid x = bar ();
  return 0;
}
