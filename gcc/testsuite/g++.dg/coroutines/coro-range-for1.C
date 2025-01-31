// PR c++/118491
// { dg-do compile { target c++20 } }

#include <coroutine>

struct task {
  struct promise_type {
    task get_return_object() { return {}; }
    std::suspend_always initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    std::suspend_always yield_value(double value) { return {}; }
    void unhandled_exception() { throw; }
  };
};

task do_task() {
  const int arr[]{1, 2, 3};

  // No ICE if classic loop and not range-based one.
  // for (auto i = 0; i < 10; ++i) {

  // No ICE if these are moved out of the loop.
  // auto x = std::suspend_always{};
  // co_await x;

  for (auto _ : arr) {
    auto bar = std::suspend_always{};
    co_await bar;

    // Alternatively:
    // auto bar = 42.;
    // co_yield bar;

    // No ICE if r-values:
    // co_await std::suspend_always{};
    // co_yield 42.;
  }
}
