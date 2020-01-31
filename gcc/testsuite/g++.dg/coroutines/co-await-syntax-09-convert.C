//  { dg-additional-options "-std=c++17 -w" }

#include "coro.h"

class mycoro {
public:
  class promise_type {
  public:
    std::suspend_always initial_suspend() const noexcept { return {}; }
    std::suspend_always final_suspend() const noexcept { return {}; }
    void unhandled_exception() noexcept { }
    mycoro get_return_object() { return mycoro{}; }
  };
};

class await {
public:
  bool await_ready() const noexcept { return false; }
  bool await_suspend(std::coroutine_handle<>) noexcept {return true;}
  mycoro await_resume() { return mycoro{}; }
};

mycoro foo(mycoro source) { (void) co_await await{}; }
