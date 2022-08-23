// PR c++/102489
// { dg-do compile }
// { dg-additional-options "-O" }

#include <coroutine>

struct footask {
  struct promise_type {
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void unhandled_exception();
    void get_return_object();
  };
  std::suspend_always foo;
  footask taskfun() { co_await foo; }
};
