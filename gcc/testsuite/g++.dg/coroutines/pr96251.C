#include <coroutine>
    
struct coroutine {
  struct promise_type {
    auto get_return_object() { return coroutine(); }
    auto initial_suspend() { return std::suspend_always(); }
    auto yield_value(int) { return std::suspend_always(); }
    void return_void() {}
    auto final_suspend() noexcept { return std::suspend_always(); }
    void unhandled_exception() {}
  };
};

int main() {
  auto f = [](auto max) -> coroutine {
    for (int i = 0; i < max; ++i) {
       co_yield i;
    }
  };

  f(10);

  // From PR98976
  auto foo = [](auto&&) -> coroutine {
    switch (42) {
      case 42:
        co_return;
    }
  };
  foo(1);

}
