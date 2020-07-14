//  { dg-do run }

#include "coro.h"

struct simple {
  static inline int alive = 0;
  simple() { ++alive; }
  simple(simple&&) { ++alive; }
  ~simple() { --alive; }

  struct promise_type {
    simple get_return_object() { return simple{}; }
    void return_void() {}
    void unhandled_exception() {}
    auto initial_suspend() noexcept { return coro::suspend_never{}; }
    auto final_suspend() noexcept { return coro::suspend_never{}; }
  };
};

simple
f()
{
  co_return;
}

int main() {
  {
    f();
  }

  if (simple::alive != 0)
   {
     PRINTF ("something wrong with dtors: %d\n", simple::alive);
     abort ();
   }
  return 0;
}
