//  { dg-additional-options "-std=c++17 -w" }

#include "coro.h"

class await {
public:
  class promise_type {
  public:
    std::suspend_always initial_suspend() const noexcept { return {}; }
    std::suspend_always final_suspend() const noexcept { return {}; }
    void unhandled_exception() noexcept { }
    await get_return_object() { return await{}; }
    void return_void() {}
  };
  bool await_ready() const noexcept { return false; }
  bool await_suspend(std::coroutine_handle<>) noexcept {return true;}
  void await_resume() { }
};

class mycoro {
public:
  class promise_type {
  public:
    std::suspend_always initial_suspend() const noexcept { return {}; }
    std::suspend_always final_suspend() const noexcept { return {}; }
    void unhandled_exception() noexcept { }
    mycoro get_return_object() { return mycoro{}; }
    void return_void() {}
  };
};
mycoro foo(await awaitable) {
  co_return co_await awaitable;
}

mycoro bar()
{
 auto t = [&]() -> await { co_return; }();
 return foo (t);
}

