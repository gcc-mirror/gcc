// { dg-do run { target c++20 } }
// test case from pr106188, w/o workaround
#include <coroutine>

struct task {
  struct promise_type {
    task get_return_object() { return task{}; }
    void return_void() {}
    void unhandled_exception() {}
    auto initial_suspend() noexcept { return std::suspend_never{}; }
    auto final_suspend() noexcept { return std::suspend_never{}; }
  };
};

struct suspend_and_resume {
  bool await_ready() const { return false; }
  void await_suspend(std::coroutine_handle<> h) { h.resume(); }
  void await_resume() {}
};

task f() {
  if (co_await suspend_and_resume{}, false) {}
}

task g() {
  switch (co_await suspend_and_resume{}, 0) {
    default: break;
  }
}

int main() {
  f();
  g();
}
