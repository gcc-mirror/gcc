// { dg-additional-options "-g" }

#include <coroutine>

struct task {
  struct promise_type {
    task get_return_object() { return {}; }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
  };
  bool await_ready() { return false; }
  void await_suspend(std::coroutine_handle<> h) {}
  void await_resume() {}
};

template <typename Func>
void call(Func func) { func(); }

class foo {
  void f();
  task g();
};

void foo::f() {
  auto lambda = [this]() noexcept -> task {
      co_await g();
  };
  (void)call<decltype(lambda)>;
}

int main() {}
