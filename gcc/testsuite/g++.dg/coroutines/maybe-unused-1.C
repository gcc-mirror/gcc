// https://gcc.gnu.org/PR116502
#include <coroutine>

struct SuspendNever {
  bool await_ready() noexcept;
  void await_suspend(std::coroutine_handle<>) noexcept;
  void await_resume() noexcept;
};

struct Coroutine;

struct PromiseType {
  Coroutine get_return_object();
  SuspendNever initial_suspend();
  SuspendNever final_suspend() noexcept;
  void return_void();
  void unhandled_exception();
};

struct Coroutine {
  using promise_type = PromiseType;
};

struct Awaiter {
  bool await_ready();
  void await_suspend(std::coroutine_handle<>);
  [[nodiscard]] int& await_resume();
};

Coroutine foo()
{
  co_await Awaiter {}; // { dg-warning "Wunused-result" }
}
