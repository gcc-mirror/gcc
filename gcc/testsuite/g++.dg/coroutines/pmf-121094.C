// PR c++/121094

#include <coroutine>

struct Future {
  struct promise_type {
    Future get_return_object() { return {}; }
    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
  };
  bool await_ready() { return true; }
  void await_suspend(std::coroutine_handle<>) {}
  void await_resume() {}
};

struct Foo {
  Future test(Future (Foo::*fn)()) {
    co_await (this->*fn)();
    co_return;
  }
};
