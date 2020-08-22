#if __has_include (<coroutine>)
#include <coroutine>
using namespace std;
#elif defined (__clang__) && __has_include (<experimental/coroutine>)
#include <experimental/coroutine>
using namespace std::experimental;
#endif

struct dummy_coro
{
  using promise_type = dummy_coro;
  bool await_ready() { return false; }
  void await_suspend(std::coroutine_handle<>) { }
  void await_resume() { }
  dummy_coro get_return_object() { return {}; }
  dummy_coro initial_suspend() { return {}; }
  dummy_coro final_suspend() { return {}; }
  void return_void() { }
  void unhandled_exception() { }
};

template <int ...I>
dummy_coro
foo()
{
 ((co_await [](int){ return std::suspend_never{}; }(I)), ...);
  co_return;
}

void bar() {
  foo<1>();
}
