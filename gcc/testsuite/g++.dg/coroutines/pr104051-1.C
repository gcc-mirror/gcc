// { dg-additional-options "-fsyntax-only" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
#include <coroutine>
template <typename>
struct promise {
  auto get_return_object() {
    return std::coroutine_handle<promise>::from_promise(*this);
  }
  auto initial_suspend() { return 42.0; }
  auto final_suspend() noexcept { return true; }
  void unhandled_exception();
  void return_void ();
};
template <typename T> struct task {
  using promise_type = promise<T>;
  task(std::coroutine_handle<promise<T>>);
  bool await_ready();
  std::coroutine_handle<> await_suspend(std::coroutine_handle<>);
  T await_resume();
};
task<double> foo() { // { dg-error {'initial_suspend\(\)' awaitable type 'double' is not a structure} }
  co_return;
} 
