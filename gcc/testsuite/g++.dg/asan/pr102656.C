// PR sanitizer/102656
// { dg-do compile }
// { dg-options "-std=c++20 -fsanitize=address" }

#include <coroutine>

class promise;

struct future {
  using promise_type = promise;
  future() = default;
  int x = 0;
};

struct promise {
  future get_return_object() noexcept { return {}; }
  auto initial_suspend() noexcept { return std::suspend_never{}; }
  auto final_suspend() noexcept { return std::suspend_never{}; }
  void return_void() noexcept {}
  void unhandled_exception() {}
};

future
func ()
{
  co_return;
}
