#include <coroutine>

struct future {
  struct promise_type {
    void return_value(int) {}
    auto initial_suspend() { return std::suspend_never{}; }
    auto final_suspend() noexcept { return std::suspend_never{}; }
    void unhandled_exception() {}
    future get_return_object() { return {}; }
  };
  bool await_ready() { return true; }
  void await_suspend(std::coroutine_handle<>) {}
  int await_resume() { return 0; }
};

future co_foo() {
  for( int i = 0; i < co_await future{}; ++i );
  // ICE -------------^
  co_return 0;
}
